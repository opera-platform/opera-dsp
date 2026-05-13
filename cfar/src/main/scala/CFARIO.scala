package opera.cfar

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._
import dsptools.numbers.Real

class CFAROutput[T <: Data: Real](inputType: T, thresholdType: T, sendCut: Boolean) extends Bundle {
  requireIsChiselType(inputType)
  requireIsChiselType(thresholdType)

  val peak: Bool = Output(Bool())
  val cut: Option[T] = if (sendCut) Some(Output(inputType)) else None
  val threshold: T = Output(thresholdType)
}

class CFARIO[T <: Data: Real](val params: CFARParams[T]) extends Bundle {
  val i_data = Flipped(Decoupled(params.inputType))
  val i_last: Bool = Input(Bool())

  val i_fft_size: UInt = Input(UInt(log2Ceil(params.maxFftSize + 1).W))
  val i_threshold_scale: T = Input(params.scaleType)
  val i_log_mode: Option[Bool] = if (params.runtimeLogMode) Some(Input(Bool())) else None
  val i_noise_div_shift: UInt = Input(UInt(log2Ceil(log2Ceil(params.maxReferenceCells + 1)).W))
  val i_peak_grouping: Bool = Input(Bool())
  val i_cfar_mode: UInt = Input(UInt(2.W))
  val i_edge_policy: Option[UInt] = if (params.runtimeEdgePolicy) Some(Input(UInt(2.W))) else None
  val i_reference_cells: UInt = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
  val i_guard_cells: UInt = Input(UInt(log2Ceil(params.maxGuardCells + 1).W))

  val o_data = Decoupled(new CFAROutput(params.inputType, params.thresholdType, params.sendCut))
  val o_last: Bool = Output(Bool())
  val o_fft_bin: UInt = Output(UInt(log2Ceil(params.maxFftSize).W))
}

object CFARIO {
  def apply[T <: Data: Real](params: CFARParams[T]): CFARIO[T] = new CFARIO(params)
}
