package opera.lis

import chisel3._
import chisel3.util._

private[lis] class LISDelayLineIO[T <: Data](dataType: T, maxDepth: Int) extends Bundle {
  val i_data  = Input(dataType)
  val i_shift = Input(Bool())
  val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))

  val o_data  = Output(dataType)
}
