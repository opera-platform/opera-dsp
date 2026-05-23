package opera.cfar

import chisel3._
import dsptools.numbers._

// Ordered-statistic CFAR family (OS / GOS-CA / GOS-GO / GOS-SO). Selected by the
// unified CFAR top-level when params.cfarType == CFARType.OrderedStatistic. Picks a
// streaming or frame-buffered core based on the configured edge policy.
class GOSCFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  // Runtime edge policy and wraparound need full-frame context; static streaming policies use the LIS path.
  if (params.runtimeEdgePolicy || params.edgePolicy == CFAREdgePolicy.WrapAroundFrame) {
    val frame_core = Module(new GOSCFARFrameCore(params))
    io <> frame_core.io
  } else {
    val stream_core = Module(new GOSCFARStreamCore(params))
    io <> stream_core.io
  }
}
