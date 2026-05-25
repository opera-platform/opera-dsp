package opera.lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

class LIS[T <: Data: Real](val params: LISParams[T]) extends Module {
  params.checkSorterType()

  val io = IO(new LISIO(params))

  // Wrapper that let us choose between different LIS implementations.
  if (params.maxWindowSize == 1) {
    io.i_window_size.foreach { windowSize =>
      assert(windowSize === 1.U, "One-lane LIS requires i_window_size == 1")
    }

    val s_idle :: s_process :: s_flush :: Nil = Enum(3)
    val r_state    = RegInit(s_idle)
    val r_data     = RegInit(0.U.asTypeOf(params.dataType))
    val w_full     = r_state === s_process
    val w_flushing = r_state === s_flush

    io.i_data.ready     := !w_flushing && (!w_full || io.o_data.ready)
    io.o_data.valid     := w_flushing || (w_full && io.i_data.valid)
    io.o_data.bits      := r_data
    io.o_last           := w_flushing
    io.o_sorted_data(0) := r_data
    io.o_sorter_full    := w_full

    when(io.i_data.fire) {
      r_data  := io.i_data.bits
      r_state := Mux(io.i_last, s_flush, s_process)
    }.elsewhen(w_flushing && io.o_data.ready) {
      r_state := s_idle
    }
  } else if (params.sorterType == LISType.CntBased) {
    val u_counter_sorter = Module(new CntBasedLIS(params))
    u_counter_sorter.io <> io
  }
  else {
    val u_register_sorter =  Module(new RegBasedLIS(params))
    u_register_sorter.io <> io
  }
}
