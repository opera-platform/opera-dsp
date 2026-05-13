package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

private[cfar] class CFARFrameCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private def delayData[A <: Data](in: A, depth: Int): A = {
    if (depth == 0) in else ShiftRegister(in, depth, true.B)
  }

  private def delayValid(in: Bool, depth: Int): Bool = {
    if (depth == 0) {
      in
    } else {
      val r_pipe = RegInit(VecInit(Seq.fill(depth)(false.B)))
      r_pipe.head := in
      r_pipe.tail.zip(r_pipe.init).foreach { case (next, previous) => next := previous }
      r_pipe.last
    }
  }

  assert(
    io.i_fft_size > 2.U * io.i_reference_cells + 2.U * io.i_guard_cells + 1.U,
    "FFT size must be larger than the active CFAR reference, guard, and CUT cells"
  )
  assert(io.i_reference_cells > 0.U, "Number of reference cells must be greater than zero")
  assert(io.i_guard_cells > 0.U, "Number of guard cells must be greater than zero")
  if (params.runtimeEdgePolicy) {
    assert(
      io.i_edge_policy.get <= CFAREdgePolicy.WrapAroundFrame.U,
      "i_edge_policy must be a supported CFAREdgePolicy value"
    )
  }

  private val frame_idx_width = log2Ceil(params.maxFftSize)
  private val size_width = log2Ceil(params.maxFftSize + 1)
  private val threshold_pipe_stages =
    if (params.runtimeLogMode) params.addPipeStages.max(params.mulPipeStages)
    else if (params.logMode) params.addPipeStages
    else params.mulPipeStages
  private val retiming_stages = if (params.retiming) 1 else 0
  private val output_delay_stages = threshold_pipe_stages + retiming_stages
  private val output_queue_depth = output_delay_stages + 2

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
  private val r_cfg_noise_shift = Reg(Vec(2, UInt(log2Ceil(log2Ceil(params.maxReferenceCells + 1)).W)))
  private val r_cfg_mode = Reg(Vec(2, UInt(2.W)))
  private val r_cfg_edge_policy = Reg(Vec(2, UInt(2.W)))
  private val r_cfg_peak_group = Reg(Vec(2, Bool()))
  private val r_cfg_th_scale = Reg(Vec(2, params.scaleType.cloneType))
  private val r_cfg_log_mode = Reg(Vec(2, Bool()))

  private val out_queue = Module(new Queue(new CFARQueueItem(params), output_queue_depth, flow = true, pipe = true))
  private val r_out_reserve = RegInit(0.U(log2Ceil(output_queue_depth + 1).W))
  private val w_can_issue = r_out_reserve =/= output_queue_depth.U

  private val w_buf0_free = !r_buf_full(0) && !(r_processing && !r_rd_sel)
  private val w_buf1_free = !r_buf_full(1) && !(r_processing && r_rd_sel)
  private val w_wr_sel = Mux(r_writing, r_wr_sel, Mux(w_buf0_free, false.B, true.B))
  private val w_wr_free = Mux(w_wr_sel, w_buf1_free, w_buf0_free)

  io.i_data.ready := Mux(r_writing, w_wr_free, w_buf0_free || w_buf1_free)

  private val w_in_fire = io.i_data.fire
  private val w_frame_last = io.i_last || r_wr_idx === io.i_fft_size - 1.U
  private val w_edge_policy_in = if (params.runtimeEdgePolicy) io.i_edge_policy.get else params.edgePolicy.U

  if (params.runtimeEdgePolicy) {
    when(w_in_fire && r_writing) {
      assert(
        io.i_edge_policy.get === r_cfg_edge_policy(r_wr_sel),
        "i_edge_policy must remain stable within a frame"
      )
    }
  }

  when(w_in_fire) {
    when(w_wr_sel) {
      m_frame_1(r_wr_idx) := io.i_data.bits
    }.otherwise {
      m_frame_0(r_wr_idx) := io.i_data.bits
    }

    when(!r_writing) {
      r_wr_sel := w_wr_sel
      r_cfg_fft_size(w_wr_sel) := io.i_fft_size
      r_cfg_ref_cells(w_wr_sel) := io.i_reference_cells
      r_cfg_guard_cells(w_wr_sel) := io.i_guard_cells
      r_cfg_noise_shift(w_wr_sel) := io.i_noise_div_shift
      r_cfg_mode(w_wr_sel) := io.i_cfar_mode
      r_cfg_edge_policy(w_wr_sel) := w_edge_policy_in
      r_cfg_peak_group(w_wr_sel) := io.i_peak_grouping
      r_cfg_th_scale(w_wr_sel) := io.i_threshold_scale
      r_cfg_log_mode(w_wr_sel) := io.i_log_mode.map(_.asBool).getOrElse(params.logMode.B)
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
  private val w_issue_out = r_processing && w_can_issue

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
  private val w_avg_scale =
    if (params.retiming) {
      val r_avg_scale = RegNext(w_avg_pre_scale)
      r_avg_scale
    } else {
      w_avg_pre_scale
    }
  private val w_th_scale_pipe =
    if (params.retiming) {
      val r_th_scale_pipe = RegNext(w_th_scale)
      r_th_scale_pipe
    } else {
      w_th_scale
    }
  private val w_log_mode_pipe =
    if (params.retiming) {
      val r_log_mode_pipe = RegNext(w_log_mode)
      r_log_mode_pipe
    } else {
      w_log_mode
    }

  private val w_th =
    if (params.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = threshold_pipe_stages,
        numMulPipes = threshold_pipe_stages
      )) {
        val w_th_log = Wire(params.thresholdType.cloneType)
        val w_th_lin = Wire(params.thresholdType.cloneType)
        val r_log_mode_th_d = delayData(w_log_mode_pipe, threshold_pipe_stages)
        w_th_log := w_avg_scale context_+ w_th_scale_pipe
        w_th_lin := w_avg_scale context_* w_th_scale_pipe
        Mux(r_log_mode_th_d, w_th_log, w_th_lin)
      }
    } else if (params.logMode) {
      DspContext.withNumAddPipes(params.addPipeStages) {
        w_avg_scale context_+ w_th_scale_pipe
      }
    } else {
      DspContext.withNumMulPipes(params.mulPipeStages) {
        w_avg_scale context_* w_th_scale_pipe
      }
    }

  private val w_cut = sampleAt(r_rd_idx)
  private val w_prev_idx = Mux(r_rd_idx === 0.U, w_fft_size - 1.U, r_rd_idx - 1.U)(frame_idx_width - 1, 0)
  private val w_next_idx = Mux(r_rd_idx === w_fft_size - 1.U, 0.U, r_rd_idx + 1.U)(frame_idx_width - 1, 0)
  private val w_local_max = w_cut > sampleAt(w_prev_idx) && w_cut > sampleAt(w_next_idx)
  private val w_suppress_peak = w_edge_policy === CFAREdgePolicy.SuppressEdges.U && w_edge
  private val r_out_valid_d = delayValid(w_issue_out, output_delay_stages)
  private val r_cut_d = delayData(w_cut, output_delay_stages)
  private val r_local_max_d = delayData(w_local_max, output_delay_stages)
  private val r_peak_group_d = delayData(w_peak_group, output_delay_stages)
  private val r_suppress_peak_d = delayData(w_suppress_peak, output_delay_stages)
  private val r_last_d = delayData(r_rd_idx === w_fft_size - 1.U, output_delay_stages)
  private val r_fft_bin_d = delayData(r_rd_idx, output_delay_stages)
  private val w_above_th = r_cut_d > w_th
  private val w_peak = Mux(
    r_suppress_peak_d,
    false.B,
    Mux(r_peak_group_d, w_above_th && r_local_max_d, w_above_th)
  )

  out_queue.io.enq.valid := r_out_valid_d
  out_queue.io.enq.bits.peak := w_peak
  out_queue.io.enq.bits.threshold := Mux(r_suppress_peak_d, 0.U.asTypeOf(w_th), w_th)
  out_queue.io.enq.bits.last := r_last_d
  out_queue.io.enq.bits.fftBin := r_fft_bin_d
  if (params.sendCut) {
    out_queue.io.enq.bits.cut.get := r_cut_d
  }

  when(r_out_valid_d) {
    assert(out_queue.io.enq.ready, "CFAR frame output reservation overflowed")
  }

  out_queue.io.deq.ready := io.o_data.ready

  io.o_data.valid := out_queue.io.deq.valid
  io.o_data.bits.peak := out_queue.io.deq.bits.peak
  io.o_data.bits.threshold := out_queue.io.deq.bits.threshold
  if (params.sendCut) {
    io.o_data.bits.cut.get := out_queue.io.deq.bits.cut.get
  }
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

  when(w_issue_out && !out_queue.io.deq.fire) {
    r_out_reserve := r_out_reserve + 1.U
  }.elsewhen(!w_issue_out && out_queue.io.deq.fire) {
    r_out_reserve := r_out_reserve - 1.U
  }
}
