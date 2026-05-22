package opera.lis

import chisel3._
import dsptools.numbers._

class RegSorterNetwork [T <: Data: Real] (val params: LISParams[T]) extends Module {
  val io = IO(new RegSorterNetworkIO(params))

  val cellIndices                  = 0 until (params.maxWindowSize + 1)
  val w_next_data_by_cell          = Wire(Vec(params.maxWindowSize, params.dataType))
  val w_kept_before_insert_by_cell = Wire(Vec(params.maxWindowSize, Bool()))

  // The register sorter network is combinational. Each cell removes one matching oldest value from the sorted vector, 
  // then forwards next-data and insertion-position state to the following cell so the new sample is inserted exactly once.
  cellIndices.foreach {
    case (index) => {
      val u_register_cell = Module(new RegSorterCell(params, index + 1))
      if (index == 0) {
        u_register_cell.io.i_prev_kept_before_insert := true.B
        u_register_cell.io.i_data_remove.get         := io.i_data_remove
        u_register_cell.io.i_data_insert             := io.i_data_insert
        u_register_cell.io.i_current_sorted_data     := io.i_sorted_data(index)
        u_register_cell.io.i_next_sorted_data.get    := io.i_sorted_data(index + 1)
        io.o_next_sorted_data(index)                 := u_register_cell.io.o_data
        w_next_data_by_cell(index)                   := u_register_cell.io.o_next_data.get
        w_kept_before_insert_by_cell(index)          := u_register_cell.io.o_kept_before_insert
      }
      else if (index == params.maxWindowSize) {
        // The last cell places the new value when it's larger than everything in the window.
        u_register_cell.io.i_prev_kept_before_insert := w_kept_before_insert_by_cell(index - 1)
        u_register_cell.io.i_data_insert             := io.i_data_insert
        u_register_cell.io.i_current_sorted_data     := io.i_sorted_data(index)
        io.o_next_sorted_data(index)                 := u_register_cell.io.o_data
      }
      else {
        u_register_cell.io.i_prev_kept_before_insert := w_kept_before_insert_by_cell(index - 1)
        u_register_cell.io.i_data_remove.get         := io.i_data_remove
        u_register_cell.io.i_data_insert             := io.i_data_insert
        u_register_cell.io.i_current_sorted_data     := io.i_sorted_data(index)
        u_register_cell.io.i_next_sorted_data.get    := io.i_sorted_data(index + 1)
        u_register_cell.io.i_previous_next_data.get  := w_next_data_by_cell(index - 1)
        w_next_data_by_cell(index)                   := u_register_cell.io.o_next_data.get
        w_kept_before_insert_by_cell(index)          := u_register_cell.io.o_kept_before_insert
        io.o_next_sorted_data(index)                 := u_register_cell.io.o_data
      }
    }
  }
}
