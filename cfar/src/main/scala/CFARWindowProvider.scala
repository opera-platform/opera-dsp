package opera.cfar

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._
import dsptools.numbers.Real

private[cfar] class CFARWindowPayload[T <: Data: Real](val params: CFARParams[T]) extends Bundle {
  
  val fftBin      = UInt(log2Ceil(params.maxFftSize).W)                       // Original-frame bin index for the current CUT.
  val cut         = params.inputType.cloneType                                // Cell under test, centered in the active sliding window.
  val leftRefs    = Vec(params.maxReferenceCells, params.inputType.cloneType) // Left-side reference cells; inactive lanes are zeroed.
  val rightRefs   = Vec(params.maxReferenceCells, params.inputType.cloneType) // Right-side reference cells; inactive lanes are zeroed.
  val prev        = params.inputType.cloneType                                // Immediate previous neighbor used by peak grouping.
  val next        = params.inputType.cloneType                                // Immediate next neighbor used by peak grouping.
  val isLeftEdge  = Bool()                                                    // CUT is within the left wrap/edge span.
  val isRightEdge = Bool()                                                    // CUT is within the right wrap/edge span.
  val last        = Bool()                                                    // Final CUT payload for this frame.
}

private[cfar] class CFARWindowProvider[T <: Data: Real](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)
  requireIsChiselType(params.inputType)

  private val maxEdgeSpan        = params.maxReferenceCells + params.maxGuardCells
  private val maxWindowSamples   = 2 * maxEdgeSpan + 1
  private val replayCountWidth   = log2Ceil(params.maxFftSize + 2 * maxEdgeSpan + 1)
  private val outputCountWidth   = log2Ceil(params.maxFftSize + 1)
  private val activeIndexWidth   = log2Ceil(maxWindowSamples + 1)
  private val fftBinWidth        = log2Ceil(params.maxFftSize)

  // Replay stream input and aligned per-CUT window output.
  val io = IO(new Bundle {
    val i_data = Flipped(Decoupled(params.inputType))
    val i_last = Input(Bool())
    val i_cfg  = Input(new CFARRuntimeConfig(params))

    val o_window = Decoupled(new CFARWindowPayload(params))
  })

  // Sliding-window state plus a one-entry output hold register.
  private val zero                  = 0.U.asTypeOf(params.inputType)
  private val r_window              = RegInit(VecInit(Seq.fill(maxWindowSamples)(zero)))
  private val r_replay_sample_count = RegInit(0.U(replayCountWidth.W))
  private val r_output_count        = RegInit(0.U(outputCountWidth.W))
  private val r_output_valid        = RegInit(false.B)
  private val r_output_payload      = Reg(new CFARWindowPayload(params))

  private val w_output_fire = r_output_valid && io.o_window.ready
  private val w_output_done = w_output_fire && r_output_payload.last

  // Backpressure replay input when the held payload cannot advance.
  io.i_data.ready := !r_output_valid || io.o_window.ready
  private val w_input_fire = io.i_data.fire

  // Runtime window geometry: S = R + G, active window size = 2*S + 1.
  private val w_reference_cells = io.i_cfg.reference_cells.pad(activeIndexWidth)
  private val w_guard_cells = io.i_cfg.guard_cells.pad(activeIndexWidth)
  private val w_edge_span = w_reference_cells +& w_guard_cells
  private val w_required_samples_before_output = w_edge_span << 1
  private val w_active_window_size = w_required_samples_before_output +& 1.U
  private val w_active_base = maxWindowSamples.U((activeIndexWidth + 1).W) - w_active_window_size

  // Shift each replay sample into the fixed Wmax register window.
  private val w_next_window = Wire(Vec(maxWindowSamples, params.inputType))
  for (index <- 0 until maxWindowSamples - 1) {
    w_next_window(index) := r_window(index + 1)
  }
  w_next_window(maxWindowSamples - 1) := io.i_data.bits

  // Select from the runtime-sized active window, right-aligned inside Wmax.
  private def selectWindow(window: Vec[T], activePosition: UInt): T = {
    Mux1H((0 until maxWindowSamples).map { index =>
      (w_active_base +& activePosition === index.U) -> window(index)
    })
  }

  // Extract references, neighbors, CUT, and metadata for the current output bin.
  private val w_right_reference_start = w_reference_cells +& (w_guard_cells << 1) +& 1.U
  private val w_payload = Wire(new CFARWindowPayload(params))
  w_payload.fftBin      := r_output_count(fftBinWidth - 1, 0)
  w_payload.cut         := selectWindow(w_next_window, w_edge_span)
  w_payload.prev        := selectWindow(w_next_window, w_edge_span - 1.U)
  w_payload.next        := selectWindow(w_next_window, w_edge_span + 1.U)
  w_payload.isLeftEdge  := CFAREdgeUtils.isLeftEdge(r_output_count, w_edge_span)
  w_payload.isRightEdge := CFAREdgeUtils.isRightEdge(r_output_count, io.i_cfg.fft_size, w_edge_span)
  w_payload.last        := r_output_count === io.i_cfg.fft_size - 1.U
  for (index <- 0 until params.maxReferenceCells) {
    w_payload.leftRefs(index) := Mux(
      index.U < io.i_cfg.reference_cells,
      selectWindow(w_next_window, index.U),
      zero
    )
    w_payload.rightRefs(index) := Mux(
      index.U < io.i_cfg.reference_cells,
      selectWindow(w_next_window, w_right_reference_start +& index.U),
      zero
    )
  }

  // First output is legal once replay has filled left halo + CUT + right halo.
  private val w_sample_produces_window = r_replay_sample_count >= w_required_samples_before_output
  private val w_final_replay_sample = w_sample_produces_window && w_payload.last

  when(w_input_fire) {
    assert(io.i_last === w_final_replay_sample, "CFAR replay last must align with the final window sample")
  }

  // Advance window/counters, hold valid payloads under backpressure, and clear at frame end.
  when(w_output_done) {
    r_window.foreach(_ := zero)
    r_replay_sample_count := 0.U
    r_output_count        := 0.U
    r_output_valid        := false.B
  }.elsewhen(w_input_fire) {
    r_window := w_next_window
    r_replay_sample_count := r_replay_sample_count + 1.U

    when(w_sample_produces_window) {
      r_output_payload := w_payload
      r_output_valid   := true.B
      when(!w_payload.last) {
        r_output_count := r_output_count + 1.U
      }
    }.otherwise {
      r_output_valid := false.B
    }
  }.elsewhen(w_output_fire) {
    r_output_valid := false.B
  }

  io.o_window.valid := r_output_valid
  io.o_window.bits  := r_output_payload
}
