package opera.cfar

import chisel3._
import dsptools.numbers._

private[cfar] class GOSCFARStreamCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  // Derived widths and latency constants size runtime config fields and output buffering.
  private val output_delay_stages = CFARUtils.outputDelayStages(params)
  private val output_queue_depth  = CFARUtils.outputQueueDepth(params)

  // Runtime controls are staged so a load affects the next frame, not the active one.
  private val w_default_cfg = CFARRuntimeConfig.default(params)
  private val w_input_cfg   = CFARRuntimeConfig.fromIo(params, io)

  private val r_frame_active   = RegInit(false.B)
  private val r_pending_cfg    = RegInit(w_default_cfg)
  private val r_cfg            = RegInit(w_default_cfg)
  private val w_next_frame_cfg = Wire(new CFARRuntimeConfig(params))
  private val w_cfg            = Mux(r_frame_active, r_cfg, w_next_frame_cfg)
  w_next_frame_cfg    := Mux(io.i_load_cfg, w_input_cfg, r_pending_cfg)

  private val w_fft_size         = w_cfg.fft_size
  private val w_reference_cells  = w_cfg.reference_cells
  private val w_guard_cells      = w_cfg.guard_cells
  private val w_order_rank_left  = w_cfg.order_rank_left
  private val w_order_rank_right = w_cfg.order_rank_right
  private val w_cfar_mode        = w_cfg.cfar_mode
  private val w_peak_grouping    = w_cfg.peak_grouping
  private val w_threshold_scale  = w_cfg.threshold_scale

  // Runtime configuration must stay within the implementation limits.
  assert(w_fft_size <= params.maxFftSize.U, "i_fft_size exceeds maxFftSize")
  assert(w_reference_cells > 0.U, "i_reference_cells must be greater than zero")
  assert(w_reference_cells <= params.maxReferenceCells.U, "i_reference_cells exceeds maxReferenceCells")
  assert(w_guard_cells > 0.U, "i_guard_cells must be greater than zero")
  assert(w_guard_cells <= params.maxGuardCells.U, "i_guard_cells exceeds maxGuardCells")
  CFAREdgeUtils.assertActiveWindowFits(w_fft_size, w_reference_cells, w_guard_cells)
  assert(w_cfar_mode <= CFARMode.SmallestOf.U, "i_cfar_mode must be GOS-CA, GOS-GO, or GOS-SO")
  assert(w_order_rank_left > 0.U, "i_order_rank_left must be at least 1")
  assert(w_order_rank_left <= w_reference_cells, "i_order_rank_left exceeds i_reference_cells")
  assert(w_order_rank_right > 0.U, "i_order_rank_right must be at least 1")
  assert(w_order_rank_right <= w_reference_cells, "i_order_rank_right exceeds i_reference_cells")
  assert(w_cfg.edge_policy <= CFAREdgePolicy.WrapAroundFrame.U, "i_edge_policy must be a supported CFAREdgePolicy value")

  when(io.i_load_cfg) {
    r_pending_cfg := w_input_cfg
  }

  // The provider owns the LIS/delay alignment and emits one ranked CUT window per bin.
  private val rank_provider = Module(new GOSCFARLinearRankProvider(params))
  rank_provider.io.i_data             <> io.i_data
  rank_provider.io.i_last             := io.i_last
  rank_provider.io.i_fft_size         := w_fft_size
  rank_provider.io.i_reference_cells  := w_reference_cells
  rank_provider.io.i_guard_cells      := w_guard_cells
  rank_provider.io.i_order_rank_left  := w_order_rank_left
  rank_provider.io.i_order_rank_right := w_order_rank_right

  // Frame lifecycle captures config on the first accepted sample and drains after i_last.
  when(io.i_data.fire && !r_frame_active) {
    r_frame_active := true.B
    r_cfg := w_next_frame_cfg
  }

  // Select GOS-CA/GO/SO noise and apply the non-wrap edge policy.
  private val w_window          = rank_provider.io.o_window.bits
  private val w_noise_pre_scale = CFARUtils.caModeAverage(w_window.leftRank, w_window.rightRank, w_cfar_mode)
  private val (w_noise_edge, w_suppress_edge_out) = CFARUtils.selectNonWrapEdgeNoise(
    w_window.leftRank,
    w_window.rightRank,
    w_noise_pre_scale,
    w_cfg.edge_policy,
    params.runtimeEdgePolicy,
    params.edgePolicy,
    w_window.isLeftEdge,
    w_window.isRightEdge
  )

  // Threshold and peak logic reuse CA-family scaling while using rank-based GOS noise.
  private val w_th              = CFARUtils.thresholdScale(params, w_noise_edge, w_threshold_scale, w_cfg.log_mode)
  private val w_above_threshold = CFARUtils.greaterThan(w_window.cut, w_th)
  private val w_local_max       = CFARUtils.linearLocalMax(w_window.cut, w_window.prev, w_window.next, w_window.fftBin, w_fft_size)
  private val w_peak            = Mux(w_peak_grouping, w_above_threshold && w_local_max, w_above_threshold)

  // Output retiming and queue connections preserve ready/valid behavior through pipeline latency.
  private val w_output_queue =
    CFARUtils.outputQueue(
      params,
      CFARUtils.resultPayload(params, w_peak, w_th, w_window.cut, w_window.last, w_window.fftBin, w_suppress_edge_out),
      rank_provider.io.o_window.valid,
      io.o_data.ready,
      output_delay_stages,
      output_queue_depth
    )
  rank_provider.io.o_window.ready := w_output_queue.inputReady
  rank_provider.io.i_output_done  := w_output_queue.enqLastFire
  when(w_output_queue.enqLastFire) {
    r_frame_active := false.B
  }

  io.o_data.valid := w_output_queue.deq.valid
  io.o_data.bits  := w_output_queue.deq.bits.output
  io.o_last       := w_output_queue.deq.bits.last
  io.o_fft_bin    := w_output_queue.deq.bits.fftBin
}
