package opera.lis

import chisel3._

class CntSorterCellCtrl extends Module {
  val io = IO(new CntSorterCellCtrlIO)

  val w_update_cell = (io.i_current_less_than_input ^ io.i_discard_from_right) | io.i_remove_current

  io.o_update_cell         := w_update_cell
  io.o_shift_from_right    := io.i_current_less_than_input & w_update_cell
  io.o_discard_to_left     := io.i_remove_current | io.i_discard_from_right
  io.o_reset_fifo_position := w_update_cell &
                              ((io.i_left_less_than_input & !io.i_current_less_than_input) |
                              (!io.i_right_less_than_input & io.i_current_less_than_input))
}
