package opera.fft

import chisel3._
import dsptools._
import dsptools.numbers._

// Radix 2 Butterfly
object Butterfly extends hasContext {
  def apply[T <: Data: Real: BinaryRepresentation](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = {
    require(in.length == 2, f"2-point DFT expected but input sequence of length ${in.length} is found instead.")
    val a_plus_b = DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 0)) {
      in.head.context_+(in.last)
    }
    val a_minus_b = DspContext.alter(DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 0)) {
      in.head.context_-(in.last)
    }
    // Return
    Seq(a_plus_b, a_minus_b)
  }
}
