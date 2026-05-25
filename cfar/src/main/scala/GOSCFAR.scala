package opera.cfar

import chisel3._
import dsptools.numbers._

// Ordered-statistic CFAR family (OS / GOS-CA / GOS-GO / GOS-SO).
// Static non-wrap policies use the LIS streaming path directly.
// Static wraparound uses the cyclic replay path.
// Runtime edge-policy builds route each frame between them.
class GOSCFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  if (params.runtimeEdgePolicy) {
    val stream_core = Module(new GOSCFARStreamCore(params))
    val cyclic_core = Module(new GOSCFARCyclicCore(params))

    CFARUtils.connectRuntimeEdgeRouter(params, io, stream_core.io, cyclic_core.io)
  } else if (params.edgePolicy == CFAREdgePolicy.WrapAroundFrame) {
    val cyclic_core = Module(new GOSCFARCyclicCore(params))
    io <> cyclic_core.io
  } else {
    val stream_core = Module(new GOSCFARStreamCore(params))
    io <> stream_core.io
  }
}
