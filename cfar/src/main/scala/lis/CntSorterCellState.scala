package lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

class CntSorterCellState[T <: Data: Real](proto: T, sorterSize: Int) extends Bundle {
  val sorted_data        = proto.cloneType
  val fifo_position      = UInt(log2Up(sorterSize).W)
  val is_less_than_input = Bool()
}

object CntSorterCellState {
  def apply[T <: Data: Real](proto: T, sorterSize: Int): CntSorterCellState[T] =
    new CntSorterCellState(proto, sorterSize)
}
