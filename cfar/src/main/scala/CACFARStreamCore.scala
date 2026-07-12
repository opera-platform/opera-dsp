package opera.cfar

import chisel3._
import dsptools.numbers._

private[cfar] class CACFARStreamCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private val output_delay_stages = if (params.retiming) 1 else 0
  private val output_queue_depth  = CFARUtils.outputQueueDepth(params)

  // Static defaults used before the first explicit load_cfg.
  private val w_default_cfg = CFARRuntimeConfig.default(params)

  // Current CSR inputs are parked until the next frame starts.
  private val w_input_cfg = CFARRuntimeConfig.fromIo(params, io)

  private val r_pending_cfg  = RegInit(w_default_cfg)
  private val r_cfg          = RegInit(w_default_cfg)
  private val r_frame_active = RegInit(false.B)

  private val w_next_frame_cfg = Wire(new CFARRuntimeConfig(params))
  w_next_frame_cfg := Mux(io.i_load_cfg, w_input_cfg, r_pending_cfg)
  private val w_cfg = Wire(new CFARRuntimeConfig(params))
  w_cfg := Mux(r_frame_active, r_cfg, w_next_frame_cfg)

  when(io.i_load_cfg) {
    r_pending_cfg := w_input_cfg
  }

  // Runtime bounds expected by the streaming CA datapath.
  assert(w_cfg.fft_size <= params.maxFftSize.U, "i_fft_size exceeds maxFftSize")
  CFAREdgeUtils.assertActiveWindowFits(w_cfg.fft_size, w_cfg.reference_cells, w_cfg.guard_cells)
  assert(w_cfg.reference_cells > 0.U, "Number of reference cells must be greater than zero")
  assert(w_cfg.reference_cells <= params.maxReferenceCells.U, "i_reference_cells exceeds maxReferenceCells")
  assert(w_cfg.guard_cells > 0.U, "Number of guard cells must be greater than zero")
  assert(w_cfg.guard_cells <= params.maxGuardCells.U, "i_guard_cells exceeds maxGuardCells")
  assert(w_cfg.cfar_mode <= CFARMode.SmallestOf.U, "i_cfar_mode must be CA, GOCA, or SOCA")
  assert(w_cfg.edge_policy <= CFAREdgePolicy.WrapAroundFrame.U, "i_edge_policy must be a supported CFAREdgePolicy value")

  // The provider owns the linear delay chain, rolling reference sums, edge flags, and CUT/bin alignment.
  private val window_provider = Module(new CACFARLinearWindowProvider(params))
  window_provider.io.i_data            <> io.i_data
  window_provider.io.i_last            := io.i_last
  window_provider.io.i_fft_size        := w_cfg.fft_size
  window_provider.io.i_reference_cells := w_cfg.reference_cells
  window_provider.io.i_guard_cells     := w_cfg.guard_cells

  when(io.i_data.fire && !r_frame_active) {
    r_frame_active := true.B
    r_cfg := w_next_frame_cfg
  }

  // Convert active side sums into CA-family noise estimates.
  private val w_window    = window_provider.io.o_window.bits
  private val w_avg_left  = BinaryRepresentation[T].shr(w_window.leftSum, w_cfg.noise_div_shift)
  private val w_avg_right = BinaryRepresentation[T].shr(w_window.rightSum, w_cfg.noise_div_shift)
  private val w_avg_mode  = CFARUtils.caModeAverage(w_avg_left, w_avg_right, w_cfg.cfar_mode)

  // Non-wrap edge policies either suppress edge bins or use the only valid side.
  private val (w_avg_pre_scale, w_suppress_edge_out) = CFARUtils.selectNonWrapEdgeNoise(
    w_avg_left,
    w_avg_right,
    w_avg_mode,
    w_cfg.edge_policy,
    params.runtimeEdgePolicy,
    params.edgePolicy,
    w_window.isLeftEdge,
    w_window.isRightEdge
  )

  // Register the threshold inputs and all associated metadata before the DSP arithmetic.
  private val w_threshold_output_ready = Wire(Bool())
  private val (w_threshold_payload, w_threshold_valid, w_threshold_input_ready) =
    CFARUtils.thresholdInputPipeline(
      params,
      w_avg_pre_scale,
      w_cfg.threshold_scale,
      w_window.cut,
      w_window.prev,
      w_window.next,
      w_cfg.fft_size,
      w_window.fftBin,
      w_cfg.log_mode,
      w_cfg.peak_grouping,
      w_window.last,
      w_suppress_edge_out,
      window_provider.io.o_window.valid,
      w_threshold_output_ready
    )

  // Apply runtime/static threshold scaling in linear or log mode.
  private val w_threshold = CFARUtils.thresholdScale(
    params,
    w_threshold_payload.noiseEstimate,
    w_threshold_payload.thresholdScale,
    w_threshold_payload.logMode
  )

  // Linear endpoint peak grouping treats missing endpoint neighbors as automatically OK.
  private val w_local_max = CFARUtils.linearLocalMax(
    w_threshold_payload.cut,
    w_threshold_payload.prev,
    w_threshold_payload.next,
    w_threshold_payload.fftBin,
    w_threshold_payload.fftSize
  )
  private val w_above_th = CFARUtils.greaterThan(w_threshold_payload.cut, w_threshold)
  private val w_peak = Mux(
    w_threshold_payload.peakGrouping,
    w_above_th && w_local_max,
    w_above_th
  )

  private val w_output_queue =
    CFARUtils.outputQueue(
      params,
      CFARUtils.resultPayload(
        params,
        w_peak,
        w_threshold,
        w_threshold_payload.cut,
        w_threshold_payload.last,
        w_threshold_payload.fftBin,
        w_threshold_payload.suppress
      ),
      w_threshold_valid,
      io.o_data.ready,
      output_delay_stages,
      output_queue_depth
    )
  w_threshold_output_ready := w_output_queue.inputReady
  window_provider.io.o_window.ready := w_threshold_input_ready
  window_provider.io.i_output_done  := w_output_queue.enqLastFire

  when(w_output_queue.enqLastFire) {
    r_frame_active := false.B
  }

  io.o_data.valid := w_output_queue.deq.valid
  io.o_data.bits  := w_output_queue.deq.bits.output
  io.o_last       := w_output_queue.deq.bits.last
  io.o_fft_bin    := w_output_queue.deq.bits.fftBin
}
