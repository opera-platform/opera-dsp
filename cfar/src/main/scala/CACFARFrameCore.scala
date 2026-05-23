package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

private[cfar] class CACFARFrameCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private val frame_idx_width = log2Ceil(params.maxFftSize)
  private val size_width = log2Ceil(params.maxFftSize + 1)
  private val noise_shift_width = log2Ceil(log2Ceil(params.maxReferenceCells + 1))
  private val threshold_pipe_stages =
    if (params.runtimeLogMode) params.addPipeStages.max(params.mulPipeStages)
    else if (params.logMode) params.addPipeStages
    else params.mulPipeStages
  private val retiming_stages = if (params.retiming) 1 else 0
  private val output_delay_stages = threshold_pipe_stages + retiming_stages
  private val output_queue_depth = 2

  private class RuntimeConfig extends Bundle {
    val fft_size = UInt(size_width.W)
    val reference_cells = UInt(log2Ceil(params.maxReferenceCells + 1).W)
    val guard_cells = UInt(log2Ceil(params.maxGuardCells + 1).W)
    val noise_div_shift = UInt(noise_shift_width.W)
    val cfar_mode = UInt(2.W)
    val edge_policy = UInt(2.W)
    val peak_grouping = Bool()
    val threshold_scale = params.scaleType.cloneType
    val log_mode = Bool()
  }

  private val w_default_cfg = Wire(new RuntimeConfig)
  w_default_cfg.fft_size := params.maxFftSize.U
  w_default_cfg.reference_cells := params.maxReferenceCells.U
  w_default_cfg.guard_cells := params.maxGuardCells.U
  w_default_cfg.noise_div_shift := log2Ceil(params.maxReferenceCells).U(noise_shift_width.W)
  w_default_cfg.cfar_mode := CFARMode.CellAveraging.U
  w_default_cfg.edge_policy := params.edgePolicy.U
  w_default_cfg.peak_grouping := false.B
  w_default_cfg.threshold_scale := 0.U.asTypeOf(params.scaleType)
  w_default_cfg.log_mode := params.logMode.B

  private val w_input_cfg = Wire(new RuntimeConfig)
  w_input_cfg.fft_size := io.i_fft_size
  w_input_cfg.reference_cells := io.i_reference_cells
  w_input_cfg.guard_cells := io.i_guard_cells
  w_input_cfg.noise_div_shift := io.i_noise_div_shift.get
  w_input_cfg.cfar_mode := io.i_cfar_mode
  w_input_cfg.edge_policy := io.i_edge_policy.map(_.asUInt).getOrElse(params.edgePolicy.U)
  w_input_cfg.peak_grouping := io.i_peak_grouping
  w_input_cfg.threshold_scale := io.i_threshold_scale
  w_input_cfg.log_mode := io.i_log_mode.map(_.asBool).getOrElse(params.logMode.B)

  private val r_pending_cfg = RegInit(w_default_cfg)
  private val w_next_frame_cfg = Wire(new RuntimeConfig)
  w_next_frame_cfg := Mux(io.i_load_cfg, w_input_cfg, r_pending_cfg)

  when(io.i_load_cfg) {
    r_pending_cfg := w_input_cfg
  }

  assert(
    w_next_frame_cfg.fft_size > 2.U * w_next_frame_cfg.reference_cells + 2.U * w_next_frame_cfg.guard_cells + 1.U,
    "FFT size must be larger than the active CFAR reference, guard, and CUT cells"
  )
  assert(w_next_frame_cfg.reference_cells > 0.U, "Number of reference cells must be greater than zero")
  assert(w_next_frame_cfg.guard_cells > 0.U, "Number of guard cells must be greater than zero")
  if (params.runtimeEdgePolicy) {
    assert(
      w_next_frame_cfg.edge_policy <= CFAREdgePolicy.WrapAroundFrame.U,
      "i_edge_policy must be a supported CFAREdgePolicy value"
    )
  }

  private val m_frame_0 = Reg(Vec(params.maxFftSize, params.inputType.cloneType))
  private val m_frame_1 = Reg(Vec(params.maxFftSize, params.inputType.cloneType))
  private val r_buf_full = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val r_writing = RegInit(false.B)
  private val r_wr_sel = RegInit(false.B)
  private val r_wr_idx = RegInit(0.U(frame_idx_width.W))

  private val r_processing = RegInit(false.B)
  private val r_rd_sel = RegInit(false.B)
  private val r_rd_idx = RegInit(0.U(frame_idx_width.W))

  private val r_cfg_fft_size = Reg(Vec(2, UInt(size_width.W)))
  private val r_cfg_ref_cells = Reg(Vec(2, UInt(log2Ceil(params.maxReferenceCells + 1).W)))
  private val r_cfg_guard_cells = Reg(Vec(2, UInt(log2Ceil(params.maxGuardCells + 1).W)))
  private val r_cfg_noise_shift = Reg(Vec(2, UInt(noise_shift_width.W)))
  private val r_cfg_mode = Reg(Vec(2, UInt(2.W)))
  private val r_cfg_edge_policy = Reg(Vec(2, UInt(2.W)))
  private val r_cfg_peak_group = Reg(Vec(2, Bool()))
  private val r_cfg_th_scale = Reg(Vec(2, params.scaleType.cloneType))
  private val r_cfg_log_mode = Reg(Vec(2, Bool()))

  private val out_queue = Module(new Queue(new CFARQueuePayload(params), output_queue_depth, flow = true, pipe = true))
  private val w_raw_ready = Wire(Bool())

  private val w_buf0_free = !r_buf_full(0) && !(r_processing && !r_rd_sel)
  private val w_buf1_free = !r_buf_full(1) && !(r_processing && r_rd_sel)
  private val w_wr_sel = Mux(r_writing, r_wr_sel, Mux(w_buf0_free, false.B, true.B))
  private val w_wr_free = Mux(w_wr_sel, w_buf1_free, w_buf0_free)

  io.i_data.ready := Mux(r_writing, w_wr_free, w_buf0_free || w_buf1_free)

  private val w_in_fire = io.i_data.fire
  private val w_write_fft_size = Mux(r_writing, r_cfg_fft_size(r_wr_sel), w_next_frame_cfg.fft_size)
  private val w_frame_last = io.i_last || r_wr_idx === w_write_fft_size - 1.U

  when(w_in_fire) {
    when(w_wr_sel) {
      m_frame_1(r_wr_idx) := io.i_data.bits
    }.otherwise {
      m_frame_0(r_wr_idx) := io.i_data.bits
    }

    when(!r_writing) {
      r_wr_sel := w_wr_sel
      r_cfg_fft_size(w_wr_sel) := w_next_frame_cfg.fft_size
      r_cfg_ref_cells(w_wr_sel) := w_next_frame_cfg.reference_cells
      r_cfg_guard_cells(w_wr_sel) := w_next_frame_cfg.guard_cells
      r_cfg_noise_shift(w_wr_sel) := w_next_frame_cfg.noise_div_shift
      r_cfg_mode(w_wr_sel) := w_next_frame_cfg.cfar_mode
      r_cfg_edge_policy(w_wr_sel) := w_next_frame_cfg.edge_policy
      r_cfg_peak_group(w_wr_sel) := w_next_frame_cfg.peak_grouping
      r_cfg_th_scale(w_wr_sel) := w_next_frame_cfg.threshold_scale
      r_cfg_log_mode(w_wr_sel) := w_next_frame_cfg.log_mode
    }

    when(w_frame_last) {
      r_buf_full(w_wr_sel) := true.B
      r_writing := false.B
      r_wr_idx := 0.U
    }.otherwise {
      r_writing := true.B
      r_wr_idx := r_wr_idx + 1.U
    }
  }

  when(!r_processing && (r_buf_full(0) || r_buf_full(1))) {
    r_rd_sel := !r_buf_full(0)
    r_processing := true.B
    r_rd_idx := 0.U
  }

  private val w_fft_size = r_cfg_fft_size(r_rd_sel)
  private val w_ref_cells = r_cfg_ref_cells(r_rd_sel)
  private val w_guard_cells = r_cfg_guard_cells(r_rd_sel)
  private val w_noise_shift = r_cfg_noise_shift(r_rd_sel)
  private val w_cfar_mode = r_cfg_mode(r_rd_sel)
  private val w_edge_policy = r_cfg_edge_policy(r_rd_sel)
  private val w_peak_group = r_cfg_peak_group(r_rd_sel)
  private val w_th_scale = r_cfg_th_scale(r_rd_sel)
  private val w_log_mode = r_cfg_log_mode(r_rd_sel)
  private val w_issue_out = r_processing && w_raw_ready

  private def sampleAt(index: UInt): T =
    Mux(r_rd_sel, m_frame_1(index), m_frame_0(index)).asTypeOf(params.inputType)

  private def wrapped(index: UInt): UInt =
    Mux(index >= w_fft_size, index - w_fft_size, index)(frame_idx_width - 1, 0)

  private val sum_type = (io.i_data.bits * log2Ceil(params.maxReferenceCells)).cloneType
  private val w_avg_zero_sum = 0.U.asTypeOf(sum_type)

  private val w_left_terms = (0 until params.maxReferenceCells).map { index =>
    val w_term = Wire(sum_type)
    w_term := w_avg_zero_sum
    val w_raw_idx = r_rd_idx + w_fft_size + index.U - w_guard_cells - w_ref_cells
    when(index.U < w_ref_cells) {
      w_term := sampleAt(wrapped(w_raw_idx))
    }
    w_term
  }
  private val w_right_terms = (0 until params.maxReferenceCells).map { index =>
    val w_term = Wire(sum_type)
    w_term := w_avg_zero_sum
    val w_raw_idx = r_rd_idx + w_guard_cells + 1.U + index.U
    when(index.U < w_ref_cells) {
      w_term := sampleAt(wrapped(w_raw_idx))
    }
    w_term
  }

  private val w_ref_sum_left = w_left_terms.reduce(_ + _).asTypeOf(sum_type)
  private val w_ref_sum_right = w_right_terms.reduce(_ + _).asTypeOf(sum_type)
  private val w_avg_left = BinaryRepresentation[T].shr(w_ref_sum_left, w_noise_shift)
  private val w_avg_right = BinaryRepresentation[T].shr(w_ref_sum_right, w_noise_shift)
  private val w_avg_go = Mux(w_avg_left > w_avg_right, w_avg_left, w_avg_right).asTypeOf(w_avg_left.cloneType)
  private val w_avg_so = Mux(w_avg_left < w_avg_right, w_avg_left, w_avg_right).asTypeOf(w_avg_left.cloneType)
  private val w_avg_ca = BinaryRepresentation[T].shr(w_avg_right + w_avg_left, 1).asTypeOf(w_avg_left.cloneType)
  private val w_avg_mode = MuxLookup(w_cfar_mode, w_avg_so)(Seq(
    CFARMode.CellAveraging.U -> w_avg_ca,
    CFARMode.GreatestOf.U -> w_avg_go,
    CFARMode.SmallestOf.U -> w_avg_so
  ))
  private val w_avg_zero = 0.U.asTypeOf(w_avg_mode.cloneType)

  private val w_edge_span = w_ref_cells +& w_guard_cells
  private val w_left_edge = r_rd_idx < w_edge_span
  private val w_right_edge = r_rd_idx >= w_fft_size - w_edge_span
  private val w_edge = w_left_edge || w_right_edge
  private val w_avg_one_side = Mux(
    w_left_edge && !w_right_edge,
    w_avg_right,
    Mux(w_right_edge && !w_left_edge, w_avg_left, Mux(w_edge, w_avg_zero, w_avg_mode))
  )
  private val w_avg_pre_scale = MuxLookup(w_edge_policy, w_avg_one_side)(Seq(
    CFAREdgePolicy.SuppressEdges.U -> Mux(w_edge, w_avg_zero, w_avg_mode),
    CFAREdgePolicy.OneSidedAverage.U -> w_avg_one_side,
    CFAREdgePolicy.WrapAroundFrame.U -> w_avg_mode
  ))

  private val w_th =
    if (params.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = 0,
        numMulPipes = 0
      )) {
        val w_th_log = Wire(params.thresholdType.cloneType)
        val w_th_lin = Wire(params.thresholdType.cloneType)
        w_th_log := w_avg_pre_scale context_+ w_th_scale
        w_th_lin := w_avg_pre_scale context_* w_th_scale
        Mux(w_log_mode, w_th_log, w_th_lin)
      }
    } else if (params.logMode) {
      DspContext.withNumAddPipes(0) {
        w_avg_pre_scale context_+ w_th_scale
      }
    } else {
      DspContext.withNumMulPipes(0) {
        w_avg_pre_scale context_* w_th_scale
      }
    }

  private val w_cut = sampleAt(r_rd_idx)
  private val w_prev_idx = Mux(r_rd_idx === 0.U, w_fft_size - 1.U, r_rd_idx - 1.U)(frame_idx_width - 1, 0)
  private val w_next_idx = Mux(r_rd_idx === w_fft_size - 1.U, 0.U, r_rd_idx + 1.U)(frame_idx_width - 1, 0)
  private val w_local_max = w_cut > sampleAt(w_prev_idx) && w_cut > sampleAt(w_next_idx)
  private val w_suppress_peak = w_edge_policy === CFAREdgePolicy.SuppressEdges.U && w_edge
  private val w_above_th = w_cut > w_th
  private val w_peak = Mux(
    w_suppress_peak,
    false.B,
    Mux(w_peak_group, w_above_th && w_local_max, w_above_th)
  )

  private val w_raw_payload = Wire(new CFARQueuePayload(params))
  w_raw_payload.output.peak := w_peak
  w_raw_payload.output.threshold := Mux(w_suppress_peak, 0.U.asTypeOf(w_th), w_th)
  w_raw_payload.last := r_rd_idx === w_fft_size - 1.U
  w_raw_payload.fftBin := r_rd_idx
  if (params.sendCut) {
    w_raw_payload.output.cut.get := w_cut
  }

  private val (w_queue_payload, w_queue_valid, w_pipeline_ready) =
    CFARUtils.elasticPipeline(w_raw_payload, r_processing, out_queue.io.enq.ready, output_delay_stages)
  w_raw_ready := w_pipeline_ready

  out_queue.io.enq.valid := w_queue_valid
  out_queue.io.enq.bits := w_queue_payload

  out_queue.io.deq.ready := io.o_data.ready

  io.o_data.valid := out_queue.io.deq.valid
  io.o_data.bits := out_queue.io.deq.bits.output
  io.o_last := out_queue.io.deq.bits.last
  io.o_fft_bin := out_queue.io.deq.bits.fftBin

  when(w_issue_out) {
    when(r_rd_idx === w_fft_size - 1.U) {
      r_buf_full(r_rd_sel) := false.B
      r_processing := false.B
      r_rd_idx := 0.U
    }.otherwise {
      r_rd_idx := r_rd_idx + 1.U
    }
  }

}
