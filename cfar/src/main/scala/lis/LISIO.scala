package opera.lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

class LISIO[T <: Data: Real](params: LISParams[T]) extends Bundle {
  val i_data        = Flipped(Decoupled(params.dataType))
  val i_last        = Input(Bool())
  val o_data        = Decoupled(params.dataType)
  val o_last        = Output(Bool())
  val o_sorted_data = Output(Vec(params.maxWindowSize, params.dataType))
  val o_sorter_full = Output(Bool())
  val i_window_size = if (params.runTime) Some(Input(UInt(log2Ceil(params.maxWindowSize + 1).W))) else None
}
