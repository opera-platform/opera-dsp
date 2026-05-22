package opera.lis

import chisel3._
import dsptools.numbers._

class RegSorterCell [T <: Data: Real] (val params: LISParams[T], index: Int) extends Module {
  val io = IO(new RegSorterCellIO(params, index))

  // Non-final cells skip one matching removed value, then forward the local next-data and insertion-position state to the following cell.
  if (index < params.maxWindowSize + 1) {
    val w_remove_is_after_current = io.i_current_sorted_data < io.i_data_remove.get
    val w_next_data               = Mux(w_remove_is_after_current, io.i_current_sorted_data, io.i_next_sorted_data.get)
    val w_kept_before_insert      = w_next_data < io.i_data_insert
    val w_insert_at_this_cell     = if (index == 1) !w_kept_before_insert else w_kept_before_insert ^ io.i_prev_kept_before_insert

    val w_next_sorted_data = if (index == 1) {
      Mux(w_kept_before_insert, w_next_data, io.i_data_insert)
    } else {
      Mux(w_kept_before_insert, w_next_data, Mux(w_insert_at_this_cell, io.i_data_insert, io.i_previous_next_data.get))
    }

    io.o_data               := w_next_sorted_data
    io.o_kept_before_insert := w_kept_before_insert
    io.o_next_data.get      := w_next_data
  }
  else {
    // The final sentinel cell catches an insertion that belongs after all active values.
    io.o_data               := Mux(io.i_prev_kept_before_insert, io.i_data_insert, io.i_current_sorted_data)
    io.o_kept_before_insert := false.B
  }
}
