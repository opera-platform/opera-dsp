package opera.cfar

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._
import dsptools.numbers._
import opera.lis.{LIS, LISParams}

private[cfar] class GOSCFARLinearRankPayload[T <: Data: Real](val params: CFARParams[T]) extends Bundle {
  val fftBin      = UInt(log2Ceil(params.maxFftSize).W) // Original-frame bin index for this CUT.
  val cut         = params.inputType.cloneType          // Cell under test after guard/CUT alignment.
  val leftRank    = params.inputType.cloneType          // Selected rank from references before the CUT.
  val rightRank   = params.inputType.cloneType          // Selected rank from references after the CUT.
  val prev        = params.inputType.cloneType          // Linear previous neighbor for peak grouping.
  val next        = params.inputType.cloneType          // Linear next neighbor for peak grouping.
  val isLeftEdge  = Bool()                              // Left-side references would cross frame start.
  val isRightEdge = Bool()                              // Right-side references would cross frame end.
  val last        = Bool()                              // Final CUT payload for this frame.
}

private[cfar] class GOSCFARLinearRankProvider[T <: Data: Real](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)
  requireIsChiselType(params.inputType)

  private val input_bin_width  = log2Ceil(params.maxFftSize + 1)
  private val output_bin_width = log2Ceil(params.maxFftSize)

  val io = IO(new Bundle {
    val i_data             = Flipped(Decoupled(params.inputType))
    val i_last             = Input(Bool())
    val i_fft_size         = Input(UInt(log2Ceil(params.maxFftSize + 1).W))
    val i_reference_cells  = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
    val i_guard_cells      = Input(UInt(log2Ceil(params.maxGuardCells + 1).W))
    val i_order_rank_left  = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
    val i_order_rank_right = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
    val i_output_done      = Input(Bool())

    val o_window = Decoupled(new GOSCFARLinearRankPayload(params))
  })

  // Delay-chain layout: right-side refs are visible early, CUT and left-side refs emerge later.
  private val lis_params = LISParams(
    dataType      = params.inputType.cloneType,
    maxWindowSize = params.maxReferenceCells,
    sorterType    = params.lisType,
    runTime       = true
  )

  private val left_sorter = Module(new LIS(lis_params))
  left_sorter.io.i_data.bits       := io.i_data.bits
  left_sorter.io.i_last            := io.i_last
  left_sorter.io.i_window_size.get := io.i_reference_cells

  private val left_guard_delay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  left_guard_delay.io.i_data  <> left_sorter.io.o_data
  left_guard_delay.io.i_depth := io.i_guard_cells
  left_guard_delay.io.i_last  := left_sorter.io.o_last

  private val cut_delay = Module(new CFARCutDelay(params.inputType.cloneType))
  cut_delay.io.i_data <> left_guard_delay.io.o_data
  cut_delay.io.i_last := left_guard_delay.io.o_last

  private val right_guard_delay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  right_guard_delay.io.i_data  <> cut_delay.io.o_data
  right_guard_delay.io.i_depth := io.i_guard_cells
  right_guard_delay.io.i_last  := cut_delay.io.o_last

  private val right_sorter = Module(new LIS(lis_params))
  right_sorter.io.i_window_size.get := io.i_reference_cells
  right_sorter.io.i_data.bits       := right_guard_delay.io.o_data.bits
  right_sorter.io.i_data.valid      := right_guard_delay.io.o_data.valid
  right_sorter.io.i_last            := right_guard_delay.io.o_last

  // Frame counters gate warmup, tail flush, and new-frame acceptance.
  private val r_in_bin        = RegInit(0.U(input_bin_width.W))
  private val r_out_bin       = RegInit(0.U(output_bin_width.W))
  private val r_flushing      = RegInit(false.B)
  private val r_pipe_draining = RegInit(false.B)

  private val w_edge_span      = CFAREdgeUtils.edgeSpan(io.i_reference_cells, io.i_guard_cells)
  private val w_window_delay   = w_edge_span +& 1.U
  private val w_warmup_done    = r_in_bin >= w_window_delay
  private val w_raw_ready      = io.o_window.ready
  private val w_accept_allowed = !r_pipe_draining && (!w_warmup_done || w_raw_ready)

  left_sorter.io.i_data.valid := io.i_data.valid && w_accept_allowed
  io.i_data.ready             := left_sorter.io.i_data.ready && w_accept_allowed

  when(io.i_data.fire) {
    assert(io.i_last === (r_in_bin === io.i_fft_size - 1.U), "GOS-CFAR requires i_last exactly at i_fft_size - 1")
    r_in_bin := r_in_bin + 1.U
  }

  when(io.i_last && io.i_data.fire) {
    r_pipe_draining := true.B
    r_flushing      := true.B
  }

  when(io.i_output_done) {
    r_in_bin        := 0.U
    r_pipe_draining := false.B
  }

  // Raw output production starts once the right-side window has arrived, then drains after i_last.
  private val w_raw_out_valid = (w_warmup_done && io.i_data.fire) || r_flushing
  private val w_raw_last      = r_out_bin === io.i_fft_size - 1.U
  private val w_raw_out_fire  = w_raw_out_valid && w_raw_ready

  when(w_raw_out_fire) {
    r_out_bin := Mux(w_raw_last, 0.U, r_out_bin + 1.U)
  }

  when(w_raw_out_fire && w_raw_last) {
    r_flushing := false.B
  }

  // Tail-edge bins no longer need the right sorter, so the guard/CUT path can drain directly.
  private val w_right_edge = CFAREdgeUtils.isRightEdge(r_out_bin, io.i_fft_size, w_edge_span)
  cut_delay.io.o_data.ready := Mux(
    w_right_edge,
    w_raw_ready,
    Mux(right_sorter.io.o_sorter_full, right_guard_delay.io.i_data.ready, w_raw_ready)
  )
  right_guard_delay.io.o_data.ready := Mux(
    w_right_edge,
    true.B,
    Mux(right_sorter.io.o_sorter_full, right_sorter.io.i_data.ready, true.B)
  ) && w_raw_ready
  right_sorter.io.o_data.ready := w_raw_ready

  // Payload orientation names semantic sides, even though the streaming sorters are crossed in time.
  io.o_window.valid           := w_raw_out_valid
  io.o_window.bits.fftBin     := r_out_bin
  io.o_window.bits.cut        := cut_delay.io.o_data.bits
  io.o_window.bits.leftRank   := CFARUtils.selectRuntimeValue(right_sorter.io.o_sorted_data, io.i_order_rank_left)
  io.o_window.bits.rightRank  := CFARUtils.selectRuntimeValue(left_sorter.io.o_sorted_data, io.i_order_rank_right)
  io.o_window.bits.prev       := right_guard_delay.io.o_taps.head
  io.o_window.bits.next       := left_guard_delay.io.o_data.bits
  io.o_window.bits.isLeftEdge := CFAREdgeUtils.isLeftEdge(r_out_bin, w_edge_span)
  io.o_window.bits.isRightEdge := w_right_edge
  io.o_window.bits.last       := w_raw_last

  when(w_raw_out_valid) {
    assert(cut_delay.io.o_last === w_raw_last, "i_last must align with i_fft_size - 1 in GOS frame mode")
  }
}
