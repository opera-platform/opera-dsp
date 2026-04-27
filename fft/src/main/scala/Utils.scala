package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

object Utils {
  /** Build the DIF twiddle LUT address for a radix stage, including runtime odd/even size shifting. */
  def difTwiddleAddress(
      stageIndex       : Int,
      counter          : UInt,
      counterMsb       : Bool,
      noOfStages       : Int,
      runTime          : Boolean,
      isShiftedAddress : Bool,
  ): UInt = {
    val baseAddress = Cat(counterMsb, counter) + (1 << (noOfStages - stageIndex - 1)).U
    (if (runTime) Mux(isShiftedAddress, baseAddress << 1, baseAddress) else baseAddress).asTypeOf(UInt(noOfStages.W))
  }

  /** Build the DIT twiddle LUT address for a radix stage, including runtime odd/even size shifting. */
  def ditTwiddleAddress(
      stageIndex       : Int,
      counter          : UInt,
      counterMsb       : Bool,
      noOfStages       : Int,
      runTime          : Boolean,
      isShiftedAddress : Bool,
  ): UInt = {
    val baseAddress = Cat(counterMsb, counter) + (1 << (stageIndex + 1)).U
    (if (runTime) Mux(isShiftedAddress, baseAddress << 1, baseAddress) else baseAddress).asTypeOf(UInt(noOfStages.W))
  }

  /** Pipelined complex multiply with explicit DSP context settings and trim point from the input type. */
  def complexMul[T <: Data: Real: BinaryRepresentation](
      input       : DspComplex[T],
      twiddle     : DspComplex[T],
      inputType   : DspComplex[T],
      numAddPipes : Int,
      numMulPipes : Int,
      trimType    : TrimType,
      use4Muls    : Boolean,
  ): DspComplex[T] = {
    val bpos = inputType.real.cloneType match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _              => 0
    }
    DspContext.alter(DspContext.current.copy(
      numAddPipes     = numAddPipes,
      numMulPipes     = numMulPipes,
      trimType        = trimType,
      overflowType    = Grow,
      complexUse4Muls = use4Muls
    )) { input.context_*(twiddle).trimBinary(bpos) }
  }

  /** Conditionally rotate complex data by -j (swap real/imag and negate new imag). */
  def invertComplexData[T <: Data: Real](data: DspComplex[T], invertSig: Bool): DspComplex[T] = {
    val out = Wire(data.cloneType)
    out.real := Mux(invertSig,  data.imag, data.real)
    out.imag := Mux(invertSig, -data.real, data.imag)
    out
  }

  /** Drive dst from src, swapping real/imag when the direction selects IFFT ordering. */
  def assignFftOutputByDirection[T <: Data](src: DspComplex[T], dst: DspComplex[T], fftOrIfft: Bool): Unit =
    when(fftOrIfft) { dst := src }
    .otherwise      { dst.real := src.imag; dst.imag := src.real }
}
