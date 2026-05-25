package opera.cfar

import chisel3._
import dsptools.numbers._

// CFAR top-level.
// The CFAR family (Cell-Averaging or Ordered-Statistic) is selected at elaboration time via params.cfarType.
// Runtime i_cfar_mode still selects the mode within the family (CA/GOCA/SOCA, or the GOS modes).
class CFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  if (params.cfarType == CFARType.OrderedStatistic) {
    val gos = Module(new GOSCFAR(params))
    io <> gos.io
  } else {
    val ca = Module(new CACFAR(params))
    io <> ca.io
  }
}
