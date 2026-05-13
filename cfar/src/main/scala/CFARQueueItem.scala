package opera.cfar

import chisel3._
import chisel3.util._
import dsptools.numbers.Real

private[cfar] class CFARQueueItem[T <: Data: Real](params: CFARParams[T]) extends Bundle {
  val peak = Bool()
  val cut = if (params.sendCut) Some(params.inputType.cloneType) else None
  val threshold = params.thresholdType.cloneType
  val last = Bool()
  val fftBin = UInt(log2Ceil(params.maxFftSize).W)
}
