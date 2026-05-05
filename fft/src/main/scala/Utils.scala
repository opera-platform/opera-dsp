package opera.fft

import chisel3._
import dsptools._
import dsptools.numbers._
import fixedpoint._

object Utils {
  /** Pipelined complex multiply with explicit DSP context settings and trim point from the input type. */
  def complexMul(
      input       : DspComplex[FixedPoint],
      twiddle     : DspComplex[FixedPoint],
      inputType   : DspComplex[FixedPoint],
      numAddPipes : Int,
      numMulPipes : Int,
      trimType    : TrimType,
      use4Muls    : Boolean,
  ): DspComplex[FixedPoint] = {
    val bpos = inputType.real.binaryPoint.get
    DspContext.alter(DspContext.current.copy(
      numAddPipes     = numAddPipes,
      numMulPipes     = numMulPipes,
      trimType        = trimType,
      overflowType    = Grow,
      complexUse4Muls = use4Muls
    )) { input.context_*(twiddle).trimBinary(bpos) }
  }

  /** Resize complex data through normal Chisel assignment semantics. */
  def resizeComplex(data: DspComplex[FixedPoint], proto: DspComplex[FixedPoint]): DspComplex[FixedPoint] = {
    val w_out = Wire(proto)
    w_out := data
    w_out
  }

  /** True when any FixedPoint butterfly lane overflowed by one sign bit. */
  def butterflyOverflow(butterfly: Seq[DspComplex[FixedPoint]]): Bool =
    butterfly.flatMap(data => Seq(data.real, data.imag)).map { data =>
      val u = data.asUInt
      u(data.getWidth - 1) ^ u(data.getWidth - 2)
    }.foldLeft(false.B)(_ || _)

  /** Select pass-through or rounded divide-by-2 butterfly outputs. */
  def scaleButterfly(
      butterfly  : Seq[DspComplex[FixedPoint]],
      outType    : DspComplex[FixedPoint],
      divBy2     : Bool,
      growEnable : Boolean,
      trimType   : TrimType,
  ): (Seq[DspComplex[FixedPoint]], Bool) = {
    require(butterfly.length == 2, s"scaleButterfly expects two lanes, got ${butterfly.length}")
    val w_scaled = Seq.fill(2)(Wire(outType))

    if (growEnable) {
      w_scaled.zip(butterfly).foreach { case (w_out, data) => w_out := data }
      (w_scaled, false.B)
    } else {
      val divided = butterfly.map { data =>
        DspContext.alter(DspContext.current.copy(trimType = trimType, binaryPointGrowth = 0)) {
          data.div2(1)
        }
      }
      w_scaled.zip(butterfly.zip(divided)).foreach {
        case (w_out, (pass, half)) => w_out := Mux(divBy2, resizeComplex(half, outType), resizeComplex(pass, outType))
      }
      (w_scaled, butterflyOverflow(butterfly))
    }
  }

  /** Conditionally rotate complex data by -j (swap real/imag and negate new imag). */
  def invertComplexData(data: DspComplex[FixedPoint], invertSig: Bool): DspComplex[FixedPoint] = {
    val w_out = Wire(data.cloneType)
    w_out.real := Mux(invertSig,  data.imag, data.real)
    w_out.imag := Mux(invertSig, -data.real, data.imag)
    w_out
  }

  /** Drive dst from src, swapping real/imag when the direction selects IFFT ordering. */
  def assignFftOutputByDirection(src: DspComplex[FixedPoint], dst: DspComplex[FixedPoint], fftOrIfft: Bool): Unit =
    when(fftOrIfft) { dst := src }
    .otherwise      { dst.real := src.imag; dst.imag := src.real }
}
