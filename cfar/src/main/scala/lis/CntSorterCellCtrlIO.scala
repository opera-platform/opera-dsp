package lis

import chisel3._

class CntSorterCellCtrlIO extends Bundle {
  val i_current_less_than_input = Input(Bool())
  val i_left_less_than_input    = Input(Bool())
  val i_right_less_than_input   = Input(Bool())

  val i_remove_current          = Input(Bool())
  val i_discard_from_right      = Input(Bool())

  val o_discard_to_left         = Output(Bool())
  val o_update_cell             = Output(Bool())
  val o_shift_from_right        = Output(Bool())
  val o_reset_fifo_position     = Output(Bool())
}
