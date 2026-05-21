package lis

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._

private[lis] class LISDelayLine[T <: Data](val dataType: T, val maxDepth: Int, runTime: Boolean = true) extends Module {
  require(maxDepth > 1, s"Depth must be > 1, got $maxDepth")
  requireIsChiselType(dataType)

  val io = IO(new LISDelayLineIO(dataType, maxDepth))

  assert(io.i_depth <= maxDepth.U)

  private def active(index: Int): Bool =
    if (runTime) index.U < io.i_depth else true.B

  private def selectByDepth[A <: Data](i_bypass: A, r_values: Vec[A]): A =
    MuxCase(i_bypass, (1 to maxDepth).map(depth => (io.i_depth === depth.U) -> r_values(depth - 1)))

  val r_delay_full        = RegInit(false.B)
  val r_flushing_tail     = RegInit(false.B)
  val r_delay_input_count = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val r_data              = RegInit(VecInit(Seq.fill(maxDepth)(0.U.asTypeOf(dataType))))
  val r_last              = RegInit(VecInit(Seq.fill(maxDepth)(false.B)))

  val w_input_last_fire   = io.i_last && io.i_data.fire
  val w_shift_data        = io.i_data.fire || (r_flushing_tail && io.o_data.ready)
  val w_delayed_last      = selectByDepth(w_input_last_fire, r_last)

  when(w_shift_data) {
    when(active(0)) { r_data(0) := io.i_data.bits }
    for (index <- 1 until maxDepth) {
      when(active(index)) { r_data(index) := r_data(index - 1) }
    }
  }

  when(io.o_data.fire) {
    when(active(0)) { r_last(0) := w_input_last_fire }
    for (index <- 1 until maxDepth) {
      when(active(index)) { r_last(index) := r_last(index - 1) }
    }
  }

  // After i_last is accepted, continue shifting until the delayed last marker
  // exits so the sorter can drain the retained frame tail.
  when(w_input_last_fire) {
    r_flushing_tail := true.B
  }

  when(io.i_data.fire) {
    r_delay_input_count := r_delay_input_count + 1.U
  }

  when(io.i_depth > 1.U) {
    when(r_delay_input_count === io.i_depth - 1.U && io.i_data.fire) {
      r_delay_full := true.B
    }
  }.otherwise {
    when(io.i_data.fire && io.i_depth === 1.U) {
      r_delay_full := true.B
    }
  }

  when(w_delayed_last && io.o_data.fire) {
    r_delay_full        := false.B
    r_flushing_tail     := false.B
    r_delay_input_count := 0.U
  }

  io.i_data.ready := Mux(io.i_depth === 0.U, io.o_data.ready, !r_delay_full || io.o_data.ready && !r_flushing_tail)
  io.o_data.bits  := selectByDepth(io.i_data.bits, r_data)
  io.o_data.valid := Mux(io.i_depth === 0.U, io.i_data.valid, r_delay_full && io.i_data.valid || (r_flushing_tail && w_shift_data))
}
