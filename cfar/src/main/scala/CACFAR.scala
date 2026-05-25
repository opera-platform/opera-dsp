package opera.cfar

import chisel3._
import dsptools.numbers._

// Cell-Averaging CFAR family (CA / GOCA / SOCA).
// Static non-wrap policies use the streaming running-sum core directly.
// Static wraparound uses the cyclic replay core. Runtime edge-policy builds route each frame between them.
class CACFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  if (params.runtimeEdgePolicy) {
    val stream_core = Module(new CACFARStreamCore(params))
    val cyclic_core = Module(new CACFARCyclicCore(params))

    CFARUtils.connectRuntimeEdgeRouter(params, io, stream_core.io, cyclic_core.io)
  } else if (params.edgePolicy == CFAREdgePolicy.WrapAroundFrame) {
    val cyclic_core = Module(new CACFARCyclicCore(params))
    io <> cyclic_core.io
  } else {
    val stream_core = Module(new CACFARStreamCore(params))
    io <> stream_core.io
  }
}
