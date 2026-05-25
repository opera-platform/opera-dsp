package opera.cfar

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._
import dsptools.numbers._

private[cfar] object CACFARLinearWindowPayload {
  def sumType[T <: Data: Real: BinaryRepresentation](params: CFARParams[T]): T =
    CFARUtils.widenedSumType(params.inputType.cloneType, params.maxReferenceCells)
}

private[cfar] class CACFARLinearWindowPayload[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Bundle {
  private val sum_type = CACFARLinearWindowPayload.sumType(params)

  val fftBin      = UInt(log2Ceil(params.maxFftSize).W) // Original-frame bin index for this CUT.
  val cut         = params.inputType.cloneType          // Cell under test, delayed until both side windows align.
  val leftSum     = sum_type.cloneType                  // Sum of the runtime left reference cells.
  val rightSum    = sum_type.cloneType                  // Sum of the runtime right reference cells.
  val prev        = params.inputType.cloneType          // Linear previous neighbor for peak grouping.
  val next        = params.inputType.cloneType          // Linear next neighbor for peak grouping.
  val isLeftEdge  = Bool()                              // Left reference side would cross the frame start.
  val isRightEdge = Bool()                              // Right reference side would cross the frame end.
  val last        = Bool()                              // Final CUT payload for this frame.
}

private[cfar] class CACFARLinearWindowProvider[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)
  requireIsChiselType(params.inputType)

  private val input_bin_width  = log2Ceil(params.maxFftSize + 1)
  private val output_bin_width = log2Ceil(params.maxFftSize)
  private val sum_type         = CACFARLinearWindowPayload.sumType(params)

  val io = IO(new Bundle {
    val i_data            = Flipped(Decoupled(params.inputType))
    val i_last            = Input(Bool())
    val i_fft_size        = Input(UInt(log2Ceil(params.maxFftSize + 1).W))
    val i_reference_cells = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
    val i_guard_cells     = Input(UInt(log2Ceil(params.maxGuardCells + 1).W))
    val i_output_done     = Input(Bool())

    val o_window = Decoupled(new CACFARLinearWindowPayload(params))
  })

  // Delay chain layout: left refs -> left guards -> CUT -> right guards -> right refs.
  private val left_ref_delay = Module(new ReferenceDelayCells(params.inputType, params.maxReferenceCells, params.minSRAMDepth))
  left_ref_delay.io.i_data  <> io.i_data
  left_ref_delay.io.i_depth := io.i_reference_cells
  left_ref_delay.io.i_last  := io.i_last

  private val left_guard_delay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  left_guard_delay.io.i_data  <> left_ref_delay.io.o_data
  left_guard_delay.io.i_depth := io.i_guard_cells
  left_guard_delay.io.i_last  := left_ref_delay.io.o_last

  private val cut_delay = Module(new CFARCutDelay(params.inputType.cloneType))
  cut_delay.io.i_data <> left_guard_delay.io.o_data
  cut_delay.io.i_last := left_guard_delay.io.o_last

  private val right_guard_delay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  right_guard_delay.io.i_data  <> cut_delay.io.o_data
  right_guard_delay.io.i_depth := io.i_guard_cells
  right_guard_delay.io.i_last  := cut_delay.io.o_last

  private val right_ref_delay =
    Module(new ReferenceDelayCells(params.inputType, params.maxReferenceCells, params.minSRAMDepth))
  right_ref_delay.io.i_depth := io.i_reference_cells
  right_ref_delay.io.i_data.bits := right_guard_delay.io.o_data.bits
  right_ref_delay.io.i_last  := right_guard_delay.io.o_last

  // Frame progress and flush state for the non-wrap streaming window.
  private val r_in_bin        = RegInit(0.U(input_bin_width.W))
  private val r_out_bin       = RegInit(0.U(output_bin_width.W))
  private val r_flushing      = RegInit(false.B)
  private val r_cut_drain     = RegInit(false.B)
  private val r_pipe_draining = RegInit(false.B)

  private val r_ref_sum_left   = RegInit(0.U.asTypeOf(sum_type))
  private val r_ref_sum_right  = RegInit(0.U.asTypeOf(sum_type))
  private val r_output_valid   = RegInit(false.B)
  private val r_output_payload = Reg(new CACFARLinearWindowPayload(params))

  private val w_output_ready = !r_output_valid || io.o_window.ready
  private val w_raw_ready    = w_output_ready && right_ref_delay.io.i_data.ready
  private val w_output_fire  = r_output_valid && io.o_window.ready

  private val w_edge_span    = CFAREdgeUtils.edgeSpan(io.i_reference_cells, io.i_guard_cells)
  private val w_window_delay = w_edge_span +& 1.U
  private val w_warmup_done  = r_in_bin >= w_window_delay

  private val w_raw_last      = r_out_bin === io.i_fft_size - 1.U
  private val w_raw_out_valid = (w_warmup_done && io.i_data.fire) || r_flushing
  private val w_provider_output_valid = r_output_valid || w_raw_out_valid
  private val w_raw_out_fire  = w_raw_out_valid && w_raw_ready
  private val w_raw_last_fire = w_raw_out_fire && w_raw_last

  // Drive downstream delay cells from the raw output backpressure point.
  right_guard_delay.io.o_data.ready := w_raw_ready
  right_ref_delay.io.i_data.valid   := right_guard_delay.io.o_data.valid && w_raw_ready
  right_ref_delay.io.o_data.ready   := w_output_ready
  cut_delay.io.o_data.ready         := Mux(right_ref_delay.io.o_full, right_ref_delay.io.i_data.ready, w_raw_ready)

  private val w_fill_window = !w_warmup_done
  io.i_data.ready := !r_pipe_draining && (w_fill_window || w_raw_ready)

  when(io.i_data.fire) {
    r_in_bin := r_in_bin + 1.U
  }

  when(io.i_last && io.i_data.fire) {
    r_pipe_draining := true.B
    r_flushing      := true.B
  }

  when(w_raw_out_fire) {
    r_out_bin := r_out_bin + 1.U
  }

  when(w_raw_last_fire) {
    r_out_bin := 0.U
  }

  when(w_raw_last_fire) {
    r_flushing := false.B
  }

  // Reset input-side frame state only after the final payload clears downstream alignment.
  when(io.i_output_done) {
    r_in_bin        := 0.U
    r_cut_drain     := true.B
    r_pipe_draining := false.B
  }

  when(right_ref_delay.io.o_empty) {
    r_cut_drain := false.B
  }

  // Maintain rolling sums for the active left and right reference windows.
  when(w_raw_last_fire) {
    r_ref_sum_left := 0.U.asTypeOf(sum_type)
  }.elsewhen(io.i_data.fire) {
    when(left_ref_delay.io.o_full) {
      when(left_ref_delay.io.o_data.fire) {
        r_ref_sum_left := r_ref_sum_left + left_ref_delay.io.i_data.bits.asTypeOf(sum_type) - left_ref_delay.io.o_data.bits.asTypeOf(sum_type)
      }
    }.otherwise {
      r_ref_sum_left := r_ref_sum_left + left_ref_delay.io.i_data.bits.asTypeOf(sum_type)
    }
  }

  private val w_ref_sum_right_next = WireDefault(r_ref_sum_right)
  when(r_cut_drain) {
    w_ref_sum_right_next := 0.U.asTypeOf(sum_type)
  }.elsewhen(right_guard_delay.io.o_data.fire) {
    when(right_ref_delay.io.o_full) {
      when(right_ref_delay.io.o_data.fire) {
        w_ref_sum_right_next := r_ref_sum_right + right_guard_delay.io.o_data.bits.asTypeOf(sum_type) - right_ref_delay.io.o_data.bits.asTypeOf(sum_type)
      }
    }.otherwise {
      w_ref_sum_right_next := r_ref_sum_right + right_guard_delay.io.o_data.bits.asTypeOf(sum_type)
    }
  }
  when(r_cut_drain || right_guard_delay.io.o_data.fire) {
    r_ref_sum_right := w_ref_sum_right_next
  }

  // Payload fields are combinational from the held delay/sum state.
  private val w_payload = Wire(new CACFARLinearWindowPayload(params))
  w_payload.fftBin      := r_out_bin
  w_payload.cut         := cut_delay.io.o_data.bits
  w_payload.leftSum     := r_ref_sum_right
  w_payload.rightSum    := r_ref_sum_left
  w_payload.prev        := right_guard_delay.io.o_taps.head
  w_payload.next        := left_guard_delay.io.o_data.bits
  w_payload.isLeftEdge  := CFAREdgeUtils.isLeftEdge(r_out_bin, w_edge_span)
  w_payload.isRightEdge := CFAREdgeUtils.isRightEdge(r_out_bin, io.i_fft_size, w_edge_span)
  w_payload.last        := w_raw_last

  when(w_raw_out_valid) {
    assert(cut_delay.io.o_last === w_raw_last, "i_last must align with i_fft_size - 1 in frame mode")
  }

  when(w_raw_out_fire && (!io.o_window.ready || r_output_valid)) {
    r_output_payload := w_payload
    r_output_valid   := true.B
  }.elsewhen(w_output_fire) {
    r_output_valid := false.B
  }

  io.o_window.valid := w_provider_output_valid
  io.o_window.bits  := Mux(r_output_valid, r_output_payload, w_payload)
}
