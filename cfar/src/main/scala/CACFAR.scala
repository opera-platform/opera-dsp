package opera.cfar

import chisel3._
import dsptools.numbers._

// Cell-Averaging CFAR family (CA / GOCA / SOCA). Selects a streaming or a
// frame-buffered core based on the configured edge policy.
class CACFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  if (params.runtimeEdgePolicy || params.edgePolicy == CFAREdgePolicy.WrapAroundFrame) {
    val frame_core = Module(new CACFARFrameCore(params))
    io <> frame_core.io
  } else {
    val stream_core = Module(new CACFARStreamCore(params))
    io <> stream_core.io
  }
}
