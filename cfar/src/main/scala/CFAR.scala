package opera.cfar

import chisel3._
import dsptools.numbers._

class CFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private val cell_avg = Module(new CellAveragingCFAR(params))
  cell_avg.io <> io
}
