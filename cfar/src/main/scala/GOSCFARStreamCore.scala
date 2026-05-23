package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._
import opera.lis.{LIS, LISParams}

private[cfar] class GOSCFARStreamCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  private val cfarParams = params
  CFARTypeSupport.requireSupportedParams(cfarParams)

  val io: CFARIO[T] = IO(CFARIO(params))

  // Local helpers cover rank selection and GOS-CA averaging.
  private def selectRuntimeValue[A <: Data](values: Vec[A], oneBasedIndex: UInt): A = {
    Mux1H((1 to values.length).map { index =>
      (oneBasedIndex === index.U) -> values(index - 1)
    })
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

  // Derived widths and latency constants size runtime config fields and output buffering.
  private val threshold_pipe_stages =
    if (cfarParams.runtimeLogMode) cfarParams.addPipeStages.max(cfarParams.mulPipeStages)
    else if (cfarParams.logMode) cfarParams.addPipeStages
    else cfarParams.mulPipeStages
  private val retiming_stages = if (cfarParams.retiming) 1 else 0
  private val output_delay_stages = threshold_pipe_stages + retiming_stages
  private val output_queue_depth = if (cfarParams.retiming) threshold_pipe_stages + 1 else threshold_pipe_stages

  private val fftSizeWidth = log2Ceil(cfarParams.maxFftSize + 1)
  private val fftBinWidth = log2Ceil(cfarParams.maxFftSize)
  private val refWidth = log2Ceil(cfarParams.maxReferenceCells + 1)
  private val guardWidth = log2Ceil(cfarParams.maxGuardCells + 1)

  // Runtime controls are staged so a load affects the next frame, not the active one.
  private class RuntimeConfig extends Bundle {
    val fft_size = UInt(fftSizeWidth.W)
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

  private val r_frame_active = RegInit(false.B)
  private val r_pending_cfg = RegInit(w_default_cfg)
  private val r_cfg = RegInit(w_default_cfg)
  private val w_next_frame_cfg = Wire(new RuntimeConfig)
  w_next_frame_cfg := Mux(io.i_load_cfg, w_input_cfg, r_pending_cfg)
  private val w_cfg = Mux(r_frame_active, r_cfg, w_next_frame_cfg)

  private val w_fft_size = w_cfg.fft_size
  private val w_reference_cells = w_cfg.reference_cells
  private val w_guard_cells = w_cfg.guard_cells
  private val w_order_rank_left = w_cfg.order_rank_left
  private val w_order_rank_right = w_cfg.order_rank_right
  private val w_cfar_mode = w_cfg.cfar_mode
  private val w_edge_policy = w_cfg.edge_policy
  private val w_peak_grouping = w_cfg.peak_grouping
  private val w_threshold_scale = w_cfg.threshold_scale
  private val w_log_mode = w_cfg.log_mode

  // Runtime configuration must stay within the implementation limits.
  assert(w_reference_cells > 0.U, "i_reference_cells must be greater than zero")
  assert(w_reference_cells <= cfarParams.maxReferenceCells.U, "i_reference_cells exceeds maxReferenceCells")
  assert(w_guard_cells > 0.U, "i_guard_cells must be greater than zero")
  assert(w_guard_cells <= cfarParams.maxGuardCells.U, "i_guard_cells exceeds maxGuardCells")
  assert(
    w_fft_size > 2.U * w_reference_cells + 2.U * w_guard_cells + 1.U,
    "i_fft_size must be larger than the active GOS-CFAR reference, guard, and CUT cells"
  )
  assert(w_order_rank_left > 0.U, "i_order_rank_left must be at least 1")
  assert(w_order_rank_left <= w_reference_cells, "i_order_rank_left exceeds i_reference_cells")
  assert(w_order_rank_right > 0.U, "i_order_rank_right must be at least 1")
  assert(w_order_rank_right <= w_reference_cells, "i_order_rank_right exceeds i_reference_cells")
  assert(w_edge_policy <= CFAREdgePolicy.WrapAroundFrame.U, "i_edge_policy must be a supported CFAREdgePolicy value")

  when(io.i_load_cfg) {
    r_pending_cfg := w_input_cfg
  }

  // The streaming core builds side windows with LIS blocks and stores CUT samples for alignment.
  private val r_in_bin = RegInit(0.U(fftBinWidth.W))
  private val r_out_bin = RegInit(0.U(fftBinWidth.W))
  private val r_warmup_done = RegInit(false.B)
  private val r_flushing = RegInit(false.B)
  private val r_pipe_draining = RegInit(false.B)
  private val m_frame_data = Reg(Vec(cfarParams.maxFftSize, cfarParams.inputType.cloneType))

  private val w_window_delay = w_reference_cells +& w_guard_cells +& 1.U
  private val w_fill_window = !r_warmup_done && r_in_bin < w_window_delay
  private val w_raw_ready = Wire(Bool())
  private val w_accept_allowed = !r_pipe_draining && (w_fill_window || w_raw_ready)

  private val lisParams = LISParams(
    dataType = cfarParams.inputType.cloneType,
    maxWindowSize = cfarParams.maxReferenceCells,
    sorterType = params.lisType,
    runTime = true
  )

  private val left_sorter = Module(new LIS(lisParams))
  left_sorter.io.i_data.bits := io.i_data.bits
  left_sorter.io.i_data.valid := io.i_data.valid && w_accept_allowed
  left_sorter.io.i_last := io.i_last
  left_sorter.io.i_window_size.get := w_reference_cells

  private val left_guard_delay = Module(new DelayRegisterCells(cfarParams.inputType.cloneType, cfarParams.maxGuardCells))
  left_guard_delay.io.i_data <> left_sorter.io.o_data
  left_guard_delay.io.i_depth := w_guard_cells
  left_guard_delay.io.i_last := left_sorter.io.o_last

  private val cut_delay = Module(new CFARCutDelay(cfarParams.inputType.cloneType))
  cut_delay.io.i_data <> left_guard_delay.io.o_data
  cut_delay.io.i_last := left_guard_delay.io.o_last

  private val right_guard_delay = Module(new DelayRegisterCells(cfarParams.inputType.cloneType, cfarParams.maxGuardCells))
  right_guard_delay.io.i_data <> cut_delay.io.o_data
  right_guard_delay.io.i_depth := w_guard_cells
  right_guard_delay.io.i_last := cut_delay.io.o_last

  private val right_sorter = Module(new LIS(lisParams))
  right_sorter.io.i_window_size.get := w_reference_cells
  right_sorter.io.i_last := right_guard_delay.io.o_last

  private val w_raw_out_valid = Wire(Bool())
  right_sorter.io.i_data.bits := right_guard_delay.io.o_data.bits
  right_sorter.io.i_data.valid := right_guard_delay.io.o_data.valid

  io.i_data.ready := left_sorter.io.i_data.ready && w_accept_allowed
  private val w_input_fire = io.i_data.fire

  // Frame lifecycle captures config on the first accepted sample and drains after i_last.
  when(w_input_fire && !r_frame_active) {
    r_frame_active := true.B
    r_cfg := w_next_frame_cfg
  }

  when(w_input_fire) {
    m_frame_data(r_in_bin) := io.i_data.bits
    r_in_bin := r_in_bin + 1.U
  }

  when(r_in_bin === w_window_delay - 1.U && w_input_fire) {
    r_warmup_done := true.B
  }

  private val out_queue = Module(new Queue(new CFARQueuePayload(cfarParams), output_queue_depth + 1, flow = true, pipe = true))

  when(out_queue.io.enq.fire && out_queue.io.enq.bits.last) {
    r_in_bin := 0.U
    r_pipe_draining := false.B
    r_frame_active := false.B
  }

  when(io.i_last && w_input_fire) {
    r_pipe_draining := true.B
    r_flushing := true.B
  }

  w_raw_out_valid := (r_warmup_done && w_input_fire) || r_flushing

  private val w_raw_last = r_out_bin === w_fft_size - 1.U
  private val w_raw_out_fire = w_raw_out_valid && w_raw_ready
  private val w_raw_last_fire = w_raw_out_fire && w_raw_last

  when(w_raw_out_fire) {
    r_out_bin := r_out_bin + 1.U
  }

  when((r_out_bin === w_fft_size - 1.U && w_raw_out_fire) || w_raw_last_fire) {
    r_out_bin := 0.U
  }

  when(w_raw_last_fire) {
    r_flushing := false.B
  }

  when(out_queue.io.enq.fire && out_queue.io.enq.bits.last) {
    r_warmup_done := false.B
  }

  // Select side ranks and apply the configured edge policy before threshold scaling.
  private val w_edge_span = w_reference_cells +& w_guard_cells
  private val w_left_edge = r_out_bin < w_edge_span
  private val w_right_edge = r_out_bin >= w_fft_size - w_edge_span
  private val w_edge = w_left_edge || w_right_edge
  private val w_suppress_edge_out = w_edge_policy === CFAREdgePolicy.SuppressEdges.U && w_edge
  private val w_tail_edge_out = r_out_bin >= w_fft_size - w_edge_span
  private val w_left_order = selectRuntimeValue(right_sorter.io.o_sorted_data, w_order_rank_left)
  private val w_right_order = selectRuntimeValue(left_sorter.io.o_sorted_data, w_order_rank_right)
  private val w_cut = m_frame_data(r_out_bin)
  private val w_left_neighbor = selectRuntimeValue(left_guard_delay.io.o_taps, w_guard_cells)
  private val w_right_neighbor = left_guard_delay.io.o_taps.head
  private val w_local_max = w_cut > w_left_neighbor && w_cut > w_right_neighbor

  private val w_gos_go =
    Mux(w_left_order > w_right_order, w_left_order, w_right_order).asTypeOf(w_left_order.cloneType)
  private val w_gos_so =
    Mux(w_left_order < w_right_order, w_left_order, w_right_order).asTypeOf(w_left_order.cloneType)
  private val w_gos_ca = averageOrders(w_left_order, w_right_order)
  private val w_noise_pre_scale = MuxLookup(w_cfar_mode, w_gos_so)(Seq(
    CFARMode.CellAveraging.U -> w_gos_ca,
    CFARMode.GreatestOf.U -> w_gos_go,
    CFARMode.SmallestOf.U -> w_gos_so
  ))
  private val w_noise_zero = 0.U.asTypeOf(w_noise_pre_scale)
  private val w_noise_one_sided = Mux(
    w_left_edge && !w_right_edge,
    w_right_order,
    Mux(w_right_edge && !w_left_edge, w_left_order, Mux(w_edge, w_noise_zero, w_noise_pre_scale))
  )
  private val w_noise_edge = MuxLookup(w_edge_policy, w_noise_one_sided)(Seq(
    CFAREdgePolicy.SuppressEdges.U -> Mux(w_edge, w_noise_zero, w_noise_pre_scale),
    CFAREdgePolicy.OneSidedAverage.U -> w_noise_one_sided,
    CFAREdgePolicy.WrapAroundFrame.U -> w_noise_pre_scale
  ))

  // Threshold and peak logic reuse CA-family scaling while using rank-based GOS noise.
  private val w_th =
    if (cfarParams.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(numAddPipes = 0, numMulPipes = 0)) {
        val w_th_log = Wire(cfarParams.thresholdType.cloneType)
        val w_th_lin = Wire(cfarParams.thresholdType.cloneType)
        w_th_log := w_noise_edge context_+ w_threshold_scale
        w_th_lin := w_noise_edge context_* w_threshold_scale
        Mux(w_log_mode, w_th_log, w_th_lin)
      }
    } else if (cfarParams.logMode) {
      DspContext.withNumAddPipes(0) {
        w_noise_edge context_+ w_threshold_scale
      }
    } else {
      DspContext.withNumMulPipes(0) {
        w_noise_edge context_* w_threshold_scale
      }
    }

  private val w_above_threshold = w_cut > w_th
  private val w_peak = Mux(w_peak_grouping, w_above_threshold && w_local_max, w_above_threshold)

  // Output retiming and queue connections preserve ready/valid behavior through pipeline latency.
  private val w_raw_payload = Wire(new CFARQueuePayload(cfarParams))
  w_raw_payload.output.peak := Mux(w_suppress_edge_out, false.B, w_peak)
  w_raw_payload.output.threshold := Mux(w_suppress_edge_out, 0.U.asTypeOf(w_th), w_th)
  w_raw_payload.last := w_raw_last
  w_raw_payload.fftBin := r_out_bin
  if (cfarParams.sendCut) {
    w_raw_payload.output.cut.get := w_cut
  }

  private val (w_queue_payload, w_queue_valid, w_pipeline_ready) =
    CFARUtils.elasticPipeline(w_raw_payload, w_raw_out_valid, out_queue.io.enq.ready, output_delay_stages)
  w_raw_ready := w_pipeline_ready
  cut_delay.io.o_data.ready := Mux(
    w_tail_edge_out,
    w_raw_ready,
    Mux(right_sorter.io.o_sorter_full, right_guard_delay.io.i_data.ready, w_raw_ready)
  )
  right_guard_delay.io.o_data.ready := Mux(
    w_tail_edge_out,
    true.B,
    Mux(right_sorter.io.o_sorter_full, right_sorter.io.i_data.ready, true.B)
  ) && w_raw_ready
  right_sorter.io.o_data.ready := w_raw_ready

  out_queue.io.enq.valid := w_queue_valid
  out_queue.io.enq.bits := w_queue_payload
  out_queue.io.deq.ready := io.o_data.ready

  io.o_data.valid := out_queue.io.deq.valid
  io.o_data.bits := out_queue.io.deq.bits.output
  io.o_last := out_queue.io.deq.bits.last
  io.o_fft_bin := out_queue.io.deq.bits.fftBin
}
