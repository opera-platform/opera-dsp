package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

import scala.reflect.ClassTag

sealed trait DecimationType
case object DIT extends DecimationType
case object DIF extends DecimationType

/**
 * Selects the SDF FFT radix implementation at elaboration time.
 */
sealed trait SDFRadix {
  def label: String
}

/** Selects the radix-2 SDF FFT implementation. */
case object Radix2 extends SDFRadix {
  override val label = "2"
}

/** Selects the radix-2^2 SDF FFT implementation. */
case object Radix22 extends SDFRadix {
  override val label = "2^2"
}

/**
 * Parameters for configuring the streaming SDF FFT hardware.
 *
 * The public FFT input stream uses [[inDataType]]. The public FFT output stream is sized by
 * `stageDataTypes.last`. [[outDataType]] is still used internally as the final assignment and
 * narrowing type before the result is wrapped back to the public output stream type.
 *
 * @param fftSize            Number of FFT points. Must be a power of two.
 * @param twiddleType        Complex data type used for twiddle coefficients.
 * @param inDataType         Complex input stream data type.
 * @param outDataType        Internal final-output assignment and narrowing data type.
 * @param decimation         SDF decimation mode.
 * @param sdfRadix           SDF radix implementation selector.
 * @param stageDataTypes     Complex data type used by each SDF stage.
 * @param expandLogic        Per-stage growth controls. Each entry is the added integer width for that stage.
 * @param runTime            Enables runtime-selectable FFT size through `i_size`.
 * @param divBy2             Per-stage static divide-by-two controls.
 * @param divBy2Reg          Enables runtime-selectable per-stage divide-by-two controls.
 * @param overflowReg        Enables per-stage overflow status outputs.
 * @param trimType           Default trim mode used when per-stage trim arrays are not supplied.
 * @param numAddPipes        Number of pipeline registers after add/subtract operations.
 * @param numMulPipes        Number of pipeline registers after multiplication operations.
 * @param direction          Static transform direction. `true` selects FFT ordering, `false` selects IFFT ordering.
 * @param directionReg       Enables runtime-selectable transform direction through `i_fft_or_ifft`.
 * @param use4Muls           Uses the four-real-multiplier complex multiplier implementation.
 * @param useBitReverse      Inserts top-level bit reversal for natural-order streaming at both ends.
 * @param minSRAMdepth       Delay lines deeper than this threshold use SRAM-backed storage.
 * @param singlePortSRAM     Uses single-port SRAM for eligible delay lines and bit-reversal buffers.
 * @param stageTrimTypes     Optional per-stage trim mode for SDF butterfly scaling.
 * @param twiddleTrimTypes   Optional per-stage trim mode for twiddle multipliers.
 * @tparam T                 Real component data type used inside complex samples.
 */
case class FFTParams[T <: Data](
  fftSize:           Int,
  twiddleType:       DspComplex[T],
  inDataType:        DspComplex[T],
  outDataType:       DspComplex[T],
  decimation:        DecimationType,
  sdfRadix:          SDFRadix,
  stageDataTypes:    Array[DspComplex[T]],
  expandLogic:       Array[Int],
  runTime:           Boolean,
  divBy2:            Array[Boolean],
  divBy2Reg:         Boolean,
  overflowReg:       Boolean,
  trimType:          TrimType,
  numAddPipes:       Int,
  numMulPipes:       Int,
  direction:         Boolean,
  directionReg:      Boolean,
  use4Muls:          Boolean,
  useBitReverse:     Boolean,
  minSRAMdepth:      Int,
  singlePortSRAM:    Boolean,
  stageTrimTypes:    Array[TrimType] = Array.empty,
  twiddleTrimTypes:  Array[TrimType] = Array.empty) {

  require(isPow2(fftSize), "number of points must be a power of 2")

  private val stageCount = log2Up(fftSize)

  val resolvedStageTrimTypes: Array[TrimType] =
    if (stageTrimTypes.isEmpty) Array.fill(stageCount)(trimType) else stageTrimTypes
  val resolvedTwiddleTrimTypes: Array[TrimType] =
    if (twiddleTrimTypes.isEmpty) Array.fill(stageCount)(trimType) else twiddleTrimTypes

  requireStageParameterLength("stageDataTypes", stageDataTypes.length)
  requireStageParameterLength("expandLogic", expandLogic.length)
  requireStageParameterLength("divBy2", divBy2.length)
  requireStageParameterLength("stageTrimTypes", resolvedStageTrimTypes.length)
  requireStageParameterLength("twiddleTrimTypes", resolvedTwiddleTrimTypes.length)

  require(numAddPipes >= 0, s"numAddPipes must be non-negative, got $numAddPipes")
  require(numMulPipes >= 0, s"numMulPipes must be non-negative, got $numMulPipes")
  require(minSRAMdepth >= 0, s"minSRAMdepth must be non-negative, got $minSRAMdepth")

  private def requireStageParameterLength(name: String, length: Int): Unit =
    require(
      length == stageCount,
      s"$name must contain one entry per FFT stage ($stageCount entries for fftSize=$fftSize), got $length"
    )
}

/** Fixed-point FFT configuration that derives low-level [[FFTParams]]. */
final case class FixedFFTConfig(
  fftSize:             Int = 2,
  dataWidth:           Int = 16,
  binaryPoint:         Int = 14,
  outputWidth:         Int = 16,
  outputBinaryPoint:   Int = 14,
  twiddleWidth:        Int = 16,
  divBy2Reg:           Boolean = false,
  divBy2:              Seq[Boolean] = Seq.empty,
  overflowReg:         Boolean = false,
  decimation:          DecimationType = DIF,
  sdfRadix:            SDFRadix = Radix22,
  expandLogic:         Seq[Int] = Seq.empty,
  runTime:             Boolean = false,
  trimType:            TrimType = RoundHalfUp,
  stageTrimTypes:      Seq[TrimType] = Seq.empty,
  twiddleTrimTypes:    Seq[TrimType] = Seq.empty,
  numAddPipes:         Int = 0,
  numMulPipes:         Int = 0,
  direction:           Boolean = true,
  directionReg:        Boolean = false,
  use4Muls:            Boolean = false,
  useBitReverse:       Boolean = false,
  minSRAMdepth:        Int = 0,
  singlePortSRAM:      Boolean = false) {

  def toFFTParams: FFTParams[FixedPoint] = {
    require(isPow2(fftSize), "number of points must be a power of 2")

    val stageCount = log2Up(fftSize)
    val growth = perStage("expandLogic", expandLogic, 0, stageCount)
    val scaling = perStage("divBy2", divBy2, true, stageCount)

    val inDataType = DspComplex(FixedPoint(dataWidth.W, binaryPoint.BP))
    val outDataType = DspComplex(FixedPoint(outputWidth.W, outputBinaryPoint.BP))
    val twiddleType = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth - 2).BP))
    val stageDataTypes =
      growth.scanLeft(0)(_ + _).tail.map { growthBits =>
        DspComplex(FixedPoint((dataWidth + growthBits).W, binaryPoint.BP))
      }

    FFTParams(
      fftSize = fftSize,
      inDataType = inDataType,
      outDataType = outDataType,
      twiddleType = twiddleType,
      expandLogic = growth,
      stageDataTypes = stageDataTypes,
      divBy2 = scaling,
      divBy2Reg = divBy2Reg,
      overflowReg = overflowReg,
      decimation = decimation,
      sdfRadix = sdfRadix,
      runTime = runTime,
      trimType = trimType,
      stageTrimTypes = stageTrimTypes.toArray,
      twiddleTrimTypes = twiddleTrimTypes.toArray,
      numAddPipes = numAddPipes,
      numMulPipes = numMulPipes,
      direction = direction,
      directionReg = directionReg,
      use4Muls = use4Muls,
      useBitReverse = useBitReverse,
      minSRAMdepth = minSRAMdepth,
      singlePortSRAM = singlePortSRAM
    )
  }

  private def perStage[A: ClassTag](name: String, values: Seq[A], default: A, stageCount: Int): Array[A] = {
    val resolved = if (values.isEmpty) Seq.fill(stageCount)(default) else values
    require(
      resolved.length == stageCount,
      s"$name must contain one entry per FFT stage ($stageCount entries for fftSize=$fftSize), got ${resolved.length}"
    )
    resolved.toArray
  }
}
