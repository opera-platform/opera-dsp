package opera.lis

import chisel3._
import dsptools.numbers._

class LIS[T <: Data: Real](val params: LISParams[T]) extends Module {
  params.checkSorterType()

  val io = IO(new LISIO(params))

  // Wrapper that let us choose between different LIS implementations.
  if (params.sorterType == LISType.CntBased) {
    val u_counter_sorter = Module(new CntBasedLIS(params))
    u_counter_sorter.io <> io
  }
  else {
    val u_register_sorter =  Module(new RegBasedLIS(params))
    u_register_sorter.io <> io
  }
}
