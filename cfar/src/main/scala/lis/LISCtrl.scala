package opera.lis

import chisel3._
import chisel3.util._
import dsptools.numbers._

private[lis] case class LISCtrlOutput(
  w_next_state        : UInt,
  r_window_full       : Bool,
  r_active_window_size: UInt,
  w_last_active_index : UInt,
  o_last              : Bool,
  w_is_idle           : Bool,
  w_is_flushing       : Bool
)

private[lis] object LISCtrl {
  def apply[T <: Data: Real](
    params       : LISParams[T],
    i_data_fire  : Bool,
    i_out_ready  : Bool,
    i_last       : Bool,
    i_window_size: UInt
  ): LISCtrlOutput = {
    val sizeWidth  = math.max(1, log2Ceil(params.maxWindowSize + 1))
    val indexWidth = math.max(1, log2Ceil(params.maxWindowSize))

    assert(i_window_size > 0.U)
    assert(i_window_size <= params.maxWindowSize.U)

    val s_idle :: s_process :: s_flush :: Nil = Enum(3)
    val r_state              = RegInit(s_idle)
    val w_next_state         = WireDefault(r_state)
    val r_active_window_size = RegInit(params.maxWindowSize.U(sizeWidth.W))
    val w_effective_window_size = Mux(r_state === s_idle, i_window_size, r_active_window_size)
    val w_last_active_index  = (r_active_window_size - 1.U)(indexWidth - 1, 0)

    val r_input_count        = RegInit(0.U(sizeWidth.W))
    val r_flush_output_count = RegInit(0.U(indexWidth.W))
    val r_window_full        = RegInit(false.B)
    val w_input_last_fire    = i_last && i_data_fire

    // Count accepted samples until the active FIFO window is full.
    when(i_data_fire) {
      r_input_count := r_input_count + 1.U
    }.elsewhen(r_state === s_idle) {
      r_input_count := 0.U
    }

    when(r_input_count === (w_effective_window_size - 1.U) && i_data_fire) {
      r_window_full := true.B
    }.elsewhen(w_next_state === s_idle) {
      r_window_full := false.B
    }

    // During frame flush, count drained outputs so o_last marks the final beat.
    when(r_state === s_idle) {
      r_flush_output_count := 0.U
    }.elsewhen(i_out_ready && r_state === s_flush) {
      r_flush_output_count := Mux(r_flush_output_count === w_last_active_index, 0.U, r_flush_output_count + 1.U)
    }

    // Capture runtime window size while idle, then process or flush one frame.
    switch(r_state) {
      is(s_idle) {
        r_active_window_size := i_window_size
        when(i_data_fire) { w_next_state := s_process }
      }
      is(s_process) {
        when(w_input_last_fire) { w_next_state := s_flush }
      }
      is(s_flush) {
        when(i_out_ready && r_flush_output_count === w_last_active_index) { w_next_state := s_idle }
      }
    }

    r_state := w_next_state

    LISCtrlOutput(
      w_next_state         = w_next_state,
      r_window_full        = r_window_full,
      r_active_window_size = r_active_window_size,
      w_last_active_index  = w_last_active_index,
      o_last               = r_state === s_flush && r_flush_output_count === w_last_active_index,
      w_is_idle            = r_state === s_idle,
      w_is_flushing        = r_state === s_flush
    )
  }
}
