package opera.lis

import chisel3._
import dsptools.numbers._

class CntSorterCell [T <: Data: Real] (val params: LISParams[T], index: Int, emitDiscardToLeft: Boolean = true) extends Module {
  val io = IO(new CntSorterCellIO(params))

  val u_ctrl = Module(new CntSorterCellCtrl)

  val w_is_active_cell        = io.i_active.getOrElse(true.B)
  val w_is_last_active_cell   = io.i_last_cell.getOrElse((index == (params.maxWindowSize - 1)).B)
  val w_has_active_right_cell = w_is_active_cell && !w_is_last_active_cell
  val w_empty_cell_data       = LISNumeric.resetValue(params.dataType, useHigh = false)

  // Neighbor comparisons and discard propagation define whether this cell shifts from the left side, the right side, or keeps its current sorted value.
  if (index == 0) {
    u_ctrl.io.i_left_less_than_input := true.B
  }
  else {
    u_ctrl.io.i_left_less_than_input := io.i_left_cell.is_less_than_input
  }

  when(w_has_active_right_cell) {
    u_ctrl.io.i_right_less_than_input := io.i_right_cell.is_less_than_input
    u_ctrl.io.i_discard_from_right    := io.i_discard_from_right
  }
  .otherwise {
    u_ctrl.io.i_right_less_than_input := false.B
    u_ctrl.io.i_discard_from_right    := false.B
  }

  // Pick the data and FIFO position that will replace this cell when control asks it to update. 
  // Runtime inactive cells are cleared to the reset value.
  val w_update_register       = io.i_enable_sort & u_ctrl.io.o_update_cell
  val w_next_sorted_data      = Wire(params.dataType)
  val w_shifted_fifo_position = Wire(io.o_cell_state.fifo_position.cloneType)

  if (index == 0) {
    w_next_sorted_data      := Mux(w_is_last_active_cell, io.i_data, io.i_right_cell.sorted_data)
    w_shifted_fifo_position := Mux(w_is_last_active_cell, 0.U, io.i_right_cell.fifo_position)
  }
  else {
    when(w_is_last_active_cell) {
      w_next_sorted_data      := io.i_left_cell.sorted_data
      w_shifted_fifo_position := io.i_left_cell.fifo_position
    }
    .elsewhen(w_is_active_cell) {
      w_next_sorted_data      := Mux(u_ctrl.io.o_shift_from_right, io.i_right_cell.sorted_data, io.i_left_cell.sorted_data)
      w_shifted_fifo_position := Mux(u_ctrl.io.o_shift_from_right, io.i_right_cell.fifo_position, io.i_left_cell.fifo_position)
    }
    .otherwise {
      w_next_sorted_data      := w_empty_cell_data
      w_shifted_fifo_position := index.U
    }
  }

  // Registered sorted value for this cell. State 0 is the idle/reset state used between frames, while process/flush states shift values through the chain.
  val r_sorted_data = RegInit(w_empty_cell_data)

  when (io.i_state === 0.U) {
    r_sorted_data := w_empty_cell_data
  }
  .elsewhen(w_update_register) {
    r_sorted_data := w_next_sorted_data
  }

  val w_current_less_than_input = r_sorted_data < io.i_data

  // The FIFO position identifies the oldest active sample. That oldest sample is removed on the next sliding-window update.
  val w_window_size    = io.i_window_size.getOrElse(params.maxWindowSize.U)
  val r_fifo_position  = RegInit(index.U(io.o_cell_state.fifo_position.getWidth.W))
  val w_remove_current = r_fifo_position === (w_window_size - 1.U)

  // Across the active cells the FIFO positions are always a permutation of {0 .. window_size-1}.
  when(w_update_register) {
    r_fifo_position := w_shifted_fifo_position + 1.U
  }.elsewhen(io.i_enable_sort) {
    r_fifo_position := Mux(w_remove_current, 0.U, r_fifo_position + 1.U)
  }
  when(u_ctrl.io.o_reset_fifo_position && io.i_enable_sort) {
    r_fifo_position := 0.U
  }
  assert(!w_is_active_cell || r_fifo_position < w_window_size, "CntSorterCell fifo position left the {0 .. window_size-1} window")

  u_ctrl.io.i_current_less_than_input := w_current_less_than_input
  u_ctrl.io.i_remove_current := w_remove_current

  // Drive neighbor-facing values. Edge outputs are tied to zero values because there is no real cell beyond the end of the chain.
  if (index == 0 || !emitDiscardToLeft) {
    io.o_discard_to_left := false.B
  }
  else {
    io.o_discard_to_left := u_ctrl.io.o_discard_to_left
  }

  io.o_cell_state.is_less_than_input := w_current_less_than_input
  io.o_cell_state.sorted_data := r_sorted_data

  if (index == 0) {
    io.o_data_to_left := w_empty_cell_data
  }
  else {
    io.o_data_to_left := Mux(w_current_less_than_input, r_sorted_data, io.i_data)
  }

  when(w_is_active_cell) {
    io.o_data_to_right := Mux(w_current_less_than_input, io.i_data, r_sorted_data)
  }
  .otherwise {
    io.o_data_to_right := r_sorted_data
  }

  io.o_remove_current := w_remove_current
  io.o_cell_state.fifo_position := r_fifo_position
}
