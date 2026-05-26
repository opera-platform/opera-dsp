package opera.cfar

import chisel3._
import chisel3.util._
import dsptools.numbers._

private[cfar] class CACFARCyclicCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  private val output_delay_stages = CFARUtils.outputDelayStages(params)
  private val output_queue_depth  = CFARUtils.outputQueueDepth(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  // Capture full input frames and replay them with wrap samples around each side.
  private val frame_replay = Module(new CFARFrameReplay(params))
  frame_replay.io.i_data              <> io.i_data
  frame_replay.io.i_last              := io.i_last
  frame_replay.io.i_load_cfg          := io.i_load_cfg
  frame_replay.io.i_fft_size          := io.i_fft_size
  frame_replay.io.i_threshold_scale   := io.i_threshold_scale
  frame_replay.io.i_log_mode          := io.i_log_mode.getOrElse(params.logMode.B)
  frame_replay.io.i_peak_grouping     := io.i_peak_grouping
  frame_replay.io.i_cfar_mode         := io.i_cfar_mode
  frame_replay.io.i_edge_policy       := io.i_edge_policy.map(_.asUInt).getOrElse(params.edgePolicy.U)
  frame_replay.io.i_reference_cells   := io.i_reference_cells
  frame_replay.io.i_guard_cells       := io.i_guard_cells
  frame_replay.io.i_noise_div_shift   := io.i_noise_div_shift.get
  frame_replay.io.i_order_rank_left   := 1.U
  frame_replay.io.i_order_rank_right  := 1.U

  // Convert the replay stream into one aligned reference/CUT window per FFT bin.
  private val window_provider = Module(new CyclicWindowProvider(params))
  window_provider.io.i_data <> frame_replay.io.o_data
  window_provider.io.i_last := frame_replay.io.o_last
  window_provider.io.i_cfg  := frame_replay.io.o_cfg

  private val sum_type  = (window_provider.io.o_window.bits.cut * log2Ceil(params.maxReferenceCells)).cloneType

  private val w_cfg     = frame_replay.io.o_cfg
  private val w_window  = window_provider.io.o_window.bits

  // Sum only the active runtime reference lanes; unused max lanes are ignored.
  private def referenceSum(refs: Vec[T]): T = {
    val terms = (0 until params.maxReferenceCells).map { index =>
      val term = Wire(sum_type)
      term := Mux(index.U < w_cfg.reference_cells, refs(index), 0.U.asTypeOf(params.inputType)).asTypeOf(sum_type)
      term
    }
    terms.reduce(_ + _).asTypeOf(sum_type)
  }

  when(window_provider.io.o_window.valid) {
    assert(w_cfg.cfar_mode <= CFARMode.SmallestOf.U, "i_cfar_mode must be CA, GOCA, or SOCA")
    assert(w_cfg.edge_policy === CFAREdgePolicy.WrapAroundFrame.U, "CACFARCyclicCore only supports WrapAroundFrame")
  }

  // Preserve the existing CA rounding contract: shift each side, then average sides.
  private val w_left_reference_sum  = referenceSum(w_window.leftRefs)
  private val w_right_reference_sum = referenceSum(w_window.rightRefs)
  private val w_left_noise_average  = BinaryRepresentation[T].shr(w_left_reference_sum, w_cfg.noise_div_shift)
  private val w_right_noise_average = BinaryRepresentation[T].shr(w_right_reference_sum, w_cfg.noise_div_shift)
  private val w_noise_estimate      = CFARUtils.caModeAverage(w_left_noise_average, w_right_noise_average, w_cfg.cfar_mode)

  // Apply runtime/static threshold scaling in the same numeric mode as the stream core.
  private val w_threshold = CFARUtils.thresholdScale(params, w_noise_estimate, w_cfg.threshold_scale, w_cfg.log_mode)

  // Wrap peak grouping uses the circular previous/next neighbors from the window payload.
  private val w_local_max       = CFARUtils.greaterThan(w_window.cut, w_window.prev) && CFARUtils.greaterThan(w_window.cut, w_window.next)
  private val w_above_threshold = CFARUtils.greaterThan(w_window.cut, w_threshold)
  private val w_peak            = Mux(w_cfg.peak_grouping, w_above_threshold && w_local_max, w_above_threshold)

  // Package the CUT result and keep bin/last aligned through the threshold pipeline.
  private val w_output_queue =
    CFARUtils.outputQueue(
      params,
      CFARUtils.resultPayload(params, w_peak, w_threshold, w_window.cut, w_window.last, w_window.fftBin),
      window_provider.io.o_window.valid,
      io.o_data.ready,
      output_delay_stages,
      output_queue_depth
    )
  window_provider.io.o_window.ready := w_output_queue.inputReady

  io.o_data.valid := w_output_queue.deq.valid
  io.o_data.bits  := w_output_queue.deq.bits.output
  io.o_last       := w_output_queue.deq.bits.last
  io.o_fft_bin    := w_output_queue.deq.bits.fftBin

  // Release the replay buffer bank only after the final output beat is accepted.
  frame_replay.io.i_output_done := w_output_queue.deqLastFire
}
