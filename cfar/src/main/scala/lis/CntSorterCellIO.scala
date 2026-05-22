package opera.lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

class CntSorterCellIO[T <: Data: Real](params: LISParams[T]) extends Bundle {
  val i_enable_sort        = Input(Bool())
  val i_state              = Input(UInt(2.W))

  val i_left_cell          = Input(CntSorterCellState(params.dataType, params.maxWindowSize))
  val i_right_cell         = Input(CntSorterCellState(params.dataType, params.maxWindowSize))
  val o_cell_state         = Output(CntSorterCellState(params.dataType, params.maxWindowSize))

  val i_window_size        = if (params.runTime) Some(Input(UInt((log2Up(params.maxWindowSize) + 1).W))) else None
  val i_last_cell          = if (params.runTime) Some(Input(Bool())) else None
  val i_active             = if (params.runTime) Some(Input(Bool())) else None

  val i_data               = Input(params.dataType)
  val i_discard_from_right = Input(Bool())

  val o_data_to_left       = Output(params.dataType)
  val o_data_to_right      = Output(params.dataType)
  val o_remove_current     = Output(Bool())
  val o_discard_to_left    = Output(Bool())
}
