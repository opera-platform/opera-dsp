package lis

import chisel3._
import dsptools.numbers._

class RegSorterCell [T <: Data: Real] (val params: LISParams[T], index: Int) extends Module {
  val io = IO(new RegSorterCellIO(params, index))

  // Non-final cells first compact the sorted vector by skipping one matching removed value, 
  //then decide whether the compacted value belongs before the newly inserted sample.
  if (index < params.maxWindowSize + 1) {
    val w_remove_is_after_current = io.i_current_sorted_data < io.i_data_remove.get
    val w_compacted_data          = Mux(w_remove_is_after_current, io.i_current_sorted_data, io.i_next_sorted_data.get)
    val w_compacted_before_insert = w_compacted_data < io.i_data_insert
    val w_insert_at_this_cell     = if (index == 1) !w_compacted_before_insert else w_compacted_before_insert ^ io.i_prev_kept_before_insert

    val w_next_sorted_data = if (index == 1) {
      Mux(w_compacted_before_insert, w_compacted_data, io.i_data_insert)
    } else {
      Mux(w_compacted_before_insert, w_compacted_data, Mux(w_insert_at_this_cell, io.i_data_insert, io.i_previous_compacted_data.get))
    }

    io.o_data               := w_next_sorted_data
    io.o_kept_before_insert := w_compacted_before_insert
    io.o_compacted_data.get := w_compacted_data
  }
  else {
    // The final sentinel cell catches an insertion that belongs after all compacted active values.
    io.o_data               := Mux(io.i_prev_kept_before_insert, io.i_data_insert, io.i_current_sorted_data)
    io.o_kept_before_insert := false.B
  }
}
