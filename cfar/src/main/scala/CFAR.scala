package opera.cfar

import chisel3._
import dsptools.numbers._

class CFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private val cellAveraging = Module(new CellAveragingCFAR(params))
  cellAveraging.io <> io
}
