package lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

class RegBasedLIS[T <: Data: Real](val params: LISParams[T]) extends Module {
  params.checkSorterType()

  val io = IO(new LISIO(params))

  val w_sentinel_value       = LISNumeric.resetValue(params.dataType, useHigh = true)
  val w_empty_sorted_data    = VecInit(Seq.fill(params.maxWindowSize + 1)(w_sentinel_value))
  val r_extended_sorted_data = RegInit(w_empty_sorted_data)

  // Shared stream control captures runtime window size, marks when the window
  // is full, and drives tail flushing after i_last.
  val w_lis_ctrl = LISCtrl(
    params        = params,
    i_data_fire   = io.i_data.fire,
    i_out_ready   = io.o_data.ready,
    i_last        = io.i_last,
    i_window_size = io.i_window_size.getOrElse(params.maxWindowSize.U)
  )
  val w_sorted_data_for_update = Mux(w_lis_ctrl.w_is_idle, w_empty_sorted_data, r_extended_sorted_data)

  // The FIFO delay path identifies the oldest sample in the active window.
  // Register sorting removes that value before inserting i_data.
  val u_fifo_delay = Module(new LISDelayLine(params.dataType, params.maxWindowSize, runTime = params.runTime))
  u_fifo_delay.io.i_data        <> io.i_data
  u_fifo_delay.io.i_depth      := w_lis_ctrl.r_active_window_size
  u_fifo_delay.io.i_last       := io.i_last
  u_fifo_delay.io.o_data.ready := io.o_data.ready

  val w_oldest_sample = Mux(w_lis_ctrl.r_window_full, u_fifo_delay.io.o_data.bits, w_sentinel_value)

  // Combinational register sorter network computes the next sorted vector.
  // The registered vector is updated only when a new sample is accepted.
  val u_register_update_network = Module(new RegSorterNetwork(params))
  u_register_update_network.io.i_data_remove := w_oldest_sample
  u_register_update_network.io.i_data_insert := io.i_data.bits
  when (io.i_data.fire) {
    r_extended_sorted_data := u_register_update_network.io.o_next_sorted_data
  }.elsewhen(w_lis_ctrl.w_is_idle) {
    r_extended_sorted_data := w_empty_sorted_data
  }
  u_register_update_network.io.i_sorted_data := w_sorted_data_for_update

  io.o_sorter_full := w_lis_ctrl.r_window_full && !w_lis_ctrl.w_is_flushing

  io.o_sorted_data := w_sorted_data_for_update.take(params.maxWindowSize)
  io.o_data.bits   := u_fifo_delay.io.o_data.bits
  io.o_last        := w_lis_ctrl.o_last
  io.i_data.ready  := !w_lis_ctrl.r_window_full || io.o_data.ready && !w_lis_ctrl.w_is_flushing
  io.o_data.valid  := w_lis_ctrl.r_window_full && io.i_data.valid || w_lis_ctrl.w_is_flushing
}
