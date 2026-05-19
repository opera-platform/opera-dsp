package lis

import chisel3._
import dsptools.numbers._

class RegSorterCellIO[T <: Data: Real](params: LISParams[T], index: Int) extends Bundle {
  val i_prev_kept_before_insert = Input(Bool())
  val i_data_remove             = if (index == (params.maxWindowSize + 1)) None else Some(Input(params.dataType))
  val i_data_insert             = Input(params.dataType)
  val i_current_sorted_data     = Input(params.dataType)
  val i_next_sorted_data        = if (index == (params.maxWindowSize + 1)) None else Some(Input(params.dataType))
  val i_previous_compacted_data = if (index == 1 || index == (params.maxWindowSize + 1)) None else Some(Input(params.dataType))
  val o_compacted_data          = if (index == (params.maxWindowSize + 1)) None else Some(Output(params.dataType))
  val o_data                    = Output(params.dataType)
  val o_kept_before_insert      = Output(Bool())
}
