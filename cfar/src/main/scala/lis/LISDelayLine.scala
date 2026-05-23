package opera.lis

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._

// Pure controlled shift register: holds the FIFO of accepted samples and exposes the value
// that entered i_depth shifts ago (the oldest sample in the active window).
private[lis] class LISDelayLine[T <: Data](val dataType: T, val maxDepth: Int, runTime: Boolean = true) extends Module {
  require(maxDepth > 1, s"Depth must be > 1, got $maxDepth")
  requireIsChiselType(dataType)

  val io = IO(new LISDelayLineIO(dataType, maxDepth))

  assert(io.i_depth <= maxDepth.U)

  val r_data = RegInit(VecInit(Seq.fill(maxDepth)(0.U.asTypeOf(dataType))))

  when(io.i_shift) {
    r_data(0) := io.i_data
    for (index <- 1 until maxDepth) {
      r_data(index) := r_data(index - 1)
    }
  }

  // The oldest in-window sample entered i_depth shifts ago. For a fixed (non-runtime) depth the
  // tap is constant, so no per-depth select network is emitted.
  io.o_data := (
    if (runTime) MuxCase(io.i_data, (1 to maxDepth).map(depth => (io.i_depth === depth.U) -> r_data(depth - 1)))
    else r_data(maxDepth - 1)
  )
}
