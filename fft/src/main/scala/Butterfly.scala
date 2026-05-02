package opera.fft

import chisel3._
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint

// Radix 2 Butterfly
object Butterfly extends hasContext {
  def apply(in: Seq[DspComplex[FixedPoint]]): Seq[DspComplex[FixedPoint]] = {
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
