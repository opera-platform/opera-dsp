package opera.cfar

import chisel3._
import dsptools.numbers._

class CellAveragingCFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  if (params.runtimeEdgePolicy || params.edgePolicy == CFAREdgePolicy.WrapAroundFrame) {
    val frame_core = Module(new CFARFrameCore(params))
    io <> frame_core.io
  } else {
    val stream_core = Module(new CFARStreamCore(params))
    io <> stream_core.io
  }
}
