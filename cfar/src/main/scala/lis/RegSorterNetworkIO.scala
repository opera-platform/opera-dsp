package opera.lis

import chisel3._
import dsptools.numbers._

class RegSorterNetworkIO[T <: Data: Real](params: LISParams[T]) extends Bundle {
  val i_data_remove      = Input(params.dataType)
  val i_data_insert      = Input(params.dataType)
  val i_sorted_data      = Input(Vec(params.maxWindowSize + 1, params.dataType))
  val o_next_sorted_data = Output(Vec(params.maxWindowSize + 1, params.dataType))
}
