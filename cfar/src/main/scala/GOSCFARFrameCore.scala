package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

private[cfar] class GOSCFARFrameCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  private val cfarParams = params
  CFARTypeSupport.requireSupportedParams(cfarParams)

  val io: CFARIO[T] = IO(CFARIO(params))

  // Local helpers handle combinational rank selection for buffered frames.
  private def sortAscending(values: Seq[T]): Seq[T] = {
    var sorted = values
    for (pass <- values.indices) {
      for (index <- 0 until values.length - 1 - pass) {
        val left = sorted(index)
        val right = sorted(index + 1)
        val lower = Mux(left > right, right, left).asTypeOf(cfarParams.inputType)
        val upper = Mux(left > right, left, right).asTypeOf(cfarParams.inputType)
        sorted = sorted.updated(index, lower).updated(index + 1, upper)
      }
    }
    sorted
  }

  private def selectRank(values: Seq[T], activeCount: UInt, oneBasedRank: UInt): T = {
    val w_zero = Wire(cfarParams.inputType.cloneType)
    w_zero := 0.U.asTypeOf(cfarParams.inputType)

    val byActiveCount = (1 to cfarParams.maxReferenceCells).map { active =>
      val sorted = sortAscending(values.take(active))
      val selected = MuxLookup(oneBasedRank, sorted.head)(
        (1 to active).map(rank => rank.U -> sorted(rank - 1))
      ).asTypeOf(cfarParams.inputType)
      active.U -> selected
    }

    MuxLookup(activeCount, w_zero)(byActiveCount).asTypeOf(cfarParams.inputType)
  }

  private def averageOrders(left: T, right: T): T = {
    val w_avg = Wire(cfarParams.inputType.cloneType)
    (left, right) match {
      case (l: UInt, r: UInt) =>
        w_avg := ((l +& r) >> 1).asTypeOf(cfarParams.inputType)
      case (l: SInt, r: SInt) =>
        w_avg := ((l +& r) >> 1).asTypeOf(cfarParams.inputType)
      case (l: FixedPoint, r: FixedPoint) =>
        w_avg := ((l.asSInt +& r.asSInt) >> 1).asTypeOf(cfarParams.inputType)
      case _ =>
        w_avg := BinaryRepresentation[T].shr(left + right, 1).asTypeOf(cfarParams.inputType)
    }
    w_avg
  }

  // Derived widths and latency constants size frame buffers, runtime config, and output buffering.
  private val frameIdxWidth = log2Ceil(cfarParams.maxFftSize)
  private val sizeWidth = log2Ceil(cfarParams.maxFftSize + 1)
  private val refWidth = log2Ceil(cfarParams.maxReferenceCells + 1)
  private val guardWidth = log2Ceil(cfarParams.maxGuardCells + 1)
  private val thresholdPipeStages =
    if (cfarParams.runtimeLogMode) cfarParams.addPipeStages.max(cfarParams.mulPipeStages)
    else if (cfarParams.logMode) cfarParams.addPipeStages
    else cfarParams.mulPipeStages
  private val retimingStages = if (cfarParams.retiming) 1 else 0
  private val outputDelayStages = thresholdPipeStages + retimingStages
  private val outputQueueDepth = 2

  // Runtime controls are captured with each frame so processing uses stable frame-level config.
  private class RuntimeConfig extends Bundle {
    val fft_size = UInt(sizeWidth.W)
    val reference_cells = UInt(refWidth.W)
    val guard_cells = UInt(guardWidth.W)
    val order_rank_left = UInt(refWidth.W)
    val order_rank_right = UInt(refWidth.W)
    val cfar_mode = UInt(2.W)
    val edge_policy = UInt(2.W)
    val peak_grouping = Bool()
    val threshold_scale = cfarParams.scaleType.cloneType
    val log_mode = Bool()
  }

  private val w_default_cfg = Wire(new RuntimeConfig)
  w_default_cfg.fft_size := cfarParams.maxFftSize.U
  w_default_cfg.reference_cells := cfarParams.maxReferenceCells.U
  w_default_cfg.guard_cells := cfarParams.maxGuardCells.U
  w_default_cfg.order_rank_left := 1.U
  w_default_cfg.order_rank_right := 1.U
  w_default_cfg.cfar_mode := CFARMode.CellAveraging.U
  w_default_cfg.edge_policy := cfarParams.edgePolicy.U
  w_default_cfg.peak_grouping := false.B
  w_default_cfg.threshold_scale := 0.U.asTypeOf(cfarParams.scaleType)
  w_default_cfg.log_mode := cfarParams.logMode.B

  private val w_input_cfg = Wire(new RuntimeConfig)
  w_input_cfg.fft_size := io.i_fft_size
  w_input_cfg.reference_cells := io.i_reference_cells
  w_input_cfg.guard_cells := io.i_guard_cells
  w_input_cfg.order_rank_left := io.i_order_rank_left.get
  w_input_cfg.order_rank_right := io.i_order_rank_right.get
  w_input_cfg.cfar_mode := io.i_cfar_mode
  w_input_cfg.edge_policy := io.i_edge_policy.map(_.asUInt).getOrElse(cfarParams.edgePolicy.U)
  w_input_cfg.peak_grouping := io.i_peak_grouping
  w_input_cfg.threshold_scale := io.i_threshold_scale
  w_input_cfg.log_mode := io.i_log_mode.map(_.asBool).getOrElse(cfarParams.logMode.B)

  private val r_pending_cfg = RegInit(w_default_cfg)
  private val w_next_frame_cfg = Wire(new RuntimeConfig)
  w_next_frame_cfg := Mux(io.i_load_cfg, w_input_cfg, r_pending_cfg)

  when(io.i_load_cfg) {
    r_pending_cfg := w_input_cfg
  }

  // Incoming runtime configuration must stay within the configured implementation limits.
  assert(w_next_frame_cfg.reference_cells > 0.U, "i_reference_cells must be greater than zero")
  assert(w_next_frame_cfg.reference_cells <= cfarParams.maxReferenceCells.U, "i_reference_cells exceeds maxReferenceCells")
  assert(w_next_frame_cfg.guard_cells > 0.U, "i_guard_cells must be greater than zero")
  assert(w_next_frame_cfg.guard_cells <= cfarParams.maxGuardCells.U, "i_guard_cells exceeds maxGuardCells")
  assert(
    w_next_frame_cfg.fft_size > 2.U * w_next_frame_cfg.reference_cells + 2.U * w_next_frame_cfg.guard_cells + 1.U,
    "i_fft_size must be larger than the active GOS-CFAR reference, guard, and CUT cells"
  )
  assert(w_next_frame_cfg.order_rank_left > 0.U, "i_order_rank_left must be at least 1")
  assert(w_next_frame_cfg.order_rank_left <= w_next_frame_cfg.reference_cells, "i_order_rank_left exceeds i_reference_cells")
  assert(w_next_frame_cfg.order_rank_right > 0.U, "i_order_rank_right must be at least 1")
  assert(w_next_frame_cfg.order_rank_right <= w_next_frame_cfg.reference_cells, "i_order_rank_right exceeds i_reference_cells")
  assert(w_next_frame_cfg.edge_policy <= CFAREdgePolicy.WrapAroundFrame.U, "i_edge_policy must be a supported CFAREdgePolicy value")

  // Two frame buffers allow one frame to be written while another frame is being processed.
  private val m_frame_0 = Reg(Vec(cfarParams.maxFftSize, cfarParams.inputType.cloneType))
  private val m_frame_1 = Reg(Vec(cfarParams.maxFftSize, cfarParams.inputType.cloneType))
  private val r_buf_full = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val r_writing = RegInit(false.B)
  private val r_wr_sel = RegInit(false.B)
  private val r_wr_idx = RegInit(0.U(frameIdxWidth.W))

  private val r_processing = RegInit(false.B)
  private val r_rd_sel = RegInit(false.B)
  private val r_rd_idx = RegInit(0.U(frameIdxWidth.W))

  private val r_cfg_fft_size = Reg(Vec(2, UInt(sizeWidth.W)))
  private val r_cfg_ref_cells = Reg(Vec(2, UInt(refWidth.W)))
  private val r_cfg_guard_cells = Reg(Vec(2, UInt(guardWidth.W)))
  private val r_cfg_rank_left = Reg(Vec(2, UInt(refWidth.W)))
  private val r_cfg_rank_right = Reg(Vec(2, UInt(refWidth.W)))
  private val r_cfg_mode = Reg(Vec(2, UInt(2.W)))
  private val r_cfg_edge_policy = Reg(Vec(2, UInt(2.W)))
  private val r_cfg_peak_group = Reg(Vec(2, Bool()))
  private val r_cfg_th_scale = Reg(Vec(2, cfarParams.scaleType.cloneType))
  private val r_cfg_log_mode = Reg(Vec(2, Bool()))

  private val out_queue = Module(new Queue(new CFARQueuePayload(cfarParams), outputQueueDepth, flow = true, pipe = true))
  private val w_raw_ready = Wire(Bool())

  private val w_buf0_free = !r_buf_full(0) && !(r_processing && !r_rd_sel)
  private val w_buf1_free = !r_buf_full(1) && !(r_processing && r_rd_sel)
  private val w_wr_sel = Mux(r_writing, r_wr_sel, Mux(w_buf0_free, false.B, true.B))
  private val w_wr_free = Mux(w_wr_sel, w_buf1_free, w_buf0_free)

  io.i_data.ready := Mux(r_writing, w_wr_free, w_buf0_free || w_buf1_free)

  private val w_in_fire = io.i_data.fire
  private val w_frame_last =
    io.i_last || (r_wr_idx === (Mux(r_writing, r_cfg_fft_size(r_wr_sel), w_next_frame_cfg.fft_size) - 1.U))

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
      r_cfg_rank_left(w_wr_sel) := w_next_frame_cfg.order_rank_left
      r_cfg_rank_right(w_wr_sel) := w_next_frame_cfg.order_rank_right
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

  // The read side starts on the oldest full buffer and processes one stored frame at a time.
  when(!r_processing && (r_buf_full(0) || r_buf_full(1))) {
    r_rd_sel := !r_buf_full(0)
    r_processing := true.B
    r_rd_idx := 0.U
  }

  private val w_fft_size = r_cfg_fft_size(r_rd_sel)
  private val w_ref_cells = r_cfg_ref_cells(r_rd_sel)
  private val w_guard_cells = r_cfg_guard_cells(r_rd_sel)
  private val w_rank_left = r_cfg_rank_left(r_rd_sel)
  private val w_rank_right = r_cfg_rank_right(r_rd_sel)
  private val w_cfar_mode = r_cfg_mode(r_rd_sel)
  private val w_edge_policy = r_cfg_edge_policy(r_rd_sel)
  private val w_peak_group = r_cfg_peak_group(r_rd_sel)
  private val w_th_scale = r_cfg_th_scale(r_rd_sel)
  private val w_log_mode = r_cfg_log_mode(r_rd_sel)
  private val w_issue_out = r_processing && w_raw_ready

  // Reference terms wrap only inside the selected frame buffer.
  private def sampleAt(index: UInt): T =
    Mux(r_rd_sel, m_frame_1(index), m_frame_0(index)).asTypeOf(cfarParams.inputType)

  private def wrapped(index: UInt): UInt =
    Mux(index >= w_fft_size, index - w_fft_size, index)(frameIdxWidth - 1, 0)

  private val w_left_terms = (0 until cfarParams.maxReferenceCells).map { index =>
    val w_raw_idx = r_rd_idx + w_fft_size + index.U - w_guard_cells - w_ref_cells
    sampleAt(wrapped(w_raw_idx))
  }
  private val w_right_terms = (0 until cfarParams.maxReferenceCells).map { index =>
    val w_raw_idx = r_rd_idx + w_guard_cells + 1.U + index.U
    sampleAt(wrapped(w_raw_idx))
  }

  private val w_left_order = selectRank(w_left_terms, w_ref_cells, w_rank_left)
  private val w_right_order = selectRank(w_right_terms, w_ref_cells, w_rank_right)

  // Side ranks are combined by GOS mode and then adjusted for the active edge policy.
  private val w_gos_go =
    Mux(w_left_order > w_right_order, w_left_order, w_right_order).asTypeOf(w_left_order.cloneType)
  private val w_gos_so =
    Mux(w_left_order < w_right_order, w_left_order, w_right_order).asTypeOf(w_left_order.cloneType)
  private val w_gos_ca = averageOrders(w_left_order, w_right_order)
  private val w_noise_mode = MuxLookup(w_cfar_mode, w_gos_so)(Seq(
    CFARMode.CellAveraging.U -> w_gos_ca,
    CFARMode.GreatestOf.U -> w_gos_go,
    CFARMode.SmallestOf.U -> w_gos_so
  ))
  private val w_noise_zero = 0.U.asTypeOf(w_noise_mode)

  private val w_edge_span = w_ref_cells +& w_guard_cells
  private val w_left_edge = r_rd_idx < w_edge_span
  private val w_right_edge = r_rd_idx >= w_fft_size - w_edge_span
  private val w_edge = w_left_edge || w_right_edge
  private val w_noise_one_side = Mux(
    w_left_edge && !w_right_edge,
    w_right_order,
    Mux(w_right_edge && !w_left_edge, w_left_order, Mux(w_edge, w_noise_zero, w_noise_mode))
  )
  private val w_noise_pre_scale = MuxLookup(w_edge_policy, w_noise_one_side)(Seq(
    CFAREdgePolicy.SuppressEdges.U -> Mux(w_edge, w_noise_zero, w_noise_mode),
    CFAREdgePolicy.OneSidedAverage.U -> w_noise_one_side,
    CFAREdgePolicy.WrapAroundFrame.U -> w_noise_mode
  ))

  // Threshold and peak are computed for the current bin, then delayed by the elastic output path.
  private val w_th =
    if (cfarParams.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = 0,
        numMulPipes = 0
      )) {
        val w_th_log = Wire(cfarParams.thresholdType.cloneType)
        val w_th_lin = Wire(cfarParams.thresholdType.cloneType)
        w_th_log := w_noise_pre_scale context_+ w_th_scale
        w_th_lin := w_noise_pre_scale context_* w_th_scale
        Mux(w_log_mode, w_th_log, w_th_lin)
      }
    } else if (cfarParams.logMode) {
      DspContext.withNumAddPipes(0) {
        w_noise_pre_scale context_+ w_th_scale
      }
    } else {
      DspContext.withNumMulPipes(0) {
        w_noise_pre_scale context_* w_th_scale
      }
    }

  private val w_cut = sampleAt(r_rd_idx)
  private val w_prev_idx = Mux(r_rd_idx === 0.U, w_fft_size - 1.U, r_rd_idx - 1.U)(frameIdxWidth - 1, 0)
  private val w_next_idx = Mux(r_rd_idx === w_fft_size - 1.U, 0.U, r_rd_idx + 1.U)(frameIdxWidth - 1, 0)
  private val w_local_max = w_cut > sampleAt(w_prev_idx) && w_cut > sampleAt(w_next_idx)
  private val w_suppress_peak = w_edge_policy === CFAREdgePolicy.SuppressEdges.U && w_edge
  private val w_above_th = w_cut > w_th
  private val w_peak = Mux(
    w_suppress_peak,
    false.B,
    Mux(w_peak_group, w_above_th && w_local_max, w_above_th)
  )

  // The elastic output path stalls frame processing whenever the downstream queue is full.
  private val w_raw_payload = Wire(new CFARQueuePayload(cfarParams))
  w_raw_payload.output.peak := w_peak
  w_raw_payload.output.threshold := Mux(w_suppress_peak, 0.U.asTypeOf(w_th), w_th)
  w_raw_payload.last := r_rd_idx === w_fft_size - 1.U
  w_raw_payload.fftBin := r_rd_idx
  if (cfarParams.sendCut) {
    w_raw_payload.output.cut.get := w_cut
  }

  private val (w_queue_payload, w_queue_valid, w_pipeline_ready) =
    CFARUtils.elasticPipeline(w_raw_payload, r_processing, out_queue.io.enq.ready, outputDelayStages)
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
