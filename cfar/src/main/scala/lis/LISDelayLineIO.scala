package lis

import chisel3._
import chisel3.util._

private[lis] class LISDelayLineIO[T <: Data](dataType: T, maxDepth: Int) extends Bundle {
  val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))
  val i_data  = Flipped(Decoupled(dataType))
  val i_last  = Input(Bool())

  val o_data  = Decoupled(dataType)
}
