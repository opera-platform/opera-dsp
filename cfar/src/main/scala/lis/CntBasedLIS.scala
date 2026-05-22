package opera.lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

class CntBasedLIS[T <: Data: Real](val params: LISParams[T]) extends Module {
  params.checkSorterType()

  val io = IO(new LISIO(params))

  val cellIndices              = 0 until params.maxWindowSize
  val w_sorted_data            = Wire(Vec(params.maxWindowSize, params.dataType))
  val w_remove_current_by_cell = Wire(Vec(params.maxWindowSize, Bool()))

  // Shared stream control owns the active window size, frame flush state, 
  // and ready/valid behavior used by the counter sorter cell chain.
  val w_lis_ctrl = LISCtrl(
    params = params,
    i_data_fire   = io.i_data.fire,
    i_out_ready   = io.o_data.ready,
    i_last        = io.i_last,
    i_window_size = io.i_window_size.getOrElse(params.maxWindowSize.U)
  )

  // Each counter sorter cell holds one sorted value plus FIFO position metadata.
  // The chain inserts the new sample and removes the oldest active sample.
  val u_counter_cells = cellIndices.map {
    case (index) => {
      val u_cell = Module(new CntSorterCell(params, index))
      u_cell.io.i_data := io.i_data.bits
      if (params.runTime) {
        u_cell.io.i_window_size.get   := w_lis_ctrl.r_active_window_size
        u_cell.io.i_active.get        := index.U <= w_lis_ctrl.w_last_active_index
        u_cell.io.i_last_cell.get     := index.U === w_lis_ctrl.w_last_active_index
      }
      u_cell.io.i_enable_sort         := io.i_data.fire || (w_lis_ctrl.w_is_flushing && io.o_data.ready)
      u_cell.io.i_state               := w_lis_ctrl.w_next_state
      w_sorted_data(index)            := u_cell.io.o_cell_state.sorted_data
      w_remove_current_by_cell(index) := u_cell.io.o_remove_current
      u_cell
    }
  }

  // Terminate the chain edges with inactive values (0/false).
  u_counter_cells(0).io.i_left_cell.sorted_data        := 0.U.asTypeOf(params.dataType)
  u_counter_cells(0).io.i_left_cell.fifo_position      := 0.U
  u_counter_cells(0).io.i_left_cell.is_less_than_input := false.B
  u_counter_cells(params.maxWindowSize - 1).io.i_right_cell.sorted_data        := 0.U.asTypeOf(params.dataType)
  u_counter_cells(params.maxWindowSize - 1).io.i_right_cell.fifo_position      := 0.U
  u_counter_cells(params.maxWindowSize - 1).io.i_right_cell.is_less_than_input := false.B
  u_counter_cells(params.maxWindowSize - 1).io.i_discard_from_right            := false.B

  // Connect adjacent cells in both directions since data can shift left or right.
  // The oldest-sample removal propagates from the right side toward index 0.
  u_counter_cells.tail.map(_.io).foldLeft(u_counter_cells(0).io) {
    case (w_left_cell_io, w_right_cell_io) => {
      w_right_cell_io.i_left_cell.sorted_data        := w_left_cell_io.o_data_to_right
      w_right_cell_io.i_left_cell.is_less_than_input := w_left_cell_io.o_cell_state.is_less_than_input
      w_right_cell_io.i_left_cell.fifo_position      := w_left_cell_io.o_cell_state.fifo_position
      w_left_cell_io.i_right_cell.sorted_data        := w_right_cell_io.o_data_to_left
      w_left_cell_io.i_right_cell.is_less_than_input := w_right_cell_io.o_cell_state.is_less_than_input
      w_left_cell_io.i_right_cell.fifo_position      := w_right_cell_io.o_cell_state.fifo_position
      w_left_cell_io.i_discard_from_right            := w_right_cell_io.o_discard_to_left
      w_right_cell_io
    }
  }

  io.o_sorter_full := w_lis_ctrl.r_window_full && !w_lis_ctrl.w_is_flushing
  io.o_sorted_data := w_sorted_data
  io.o_data.bits   := w_sorted_data(PriorityEncoder(w_remove_current_by_cell.asUInt))
  io.o_last        := w_lis_ctrl.o_last
  io.i_data.ready  := !w_lis_ctrl.r_window_full || io.o_data.ready && !w_lis_ctrl.w_is_flushing
  io.o_data.valid  := w_lis_ctrl.r_window_full && io.i_data.valid || w_lis_ctrl.w_is_flushing
}
