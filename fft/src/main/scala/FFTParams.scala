package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

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
 * The FFT input stream uses [[inDataType]]. The FFT output stream is sized by
 * [[fftOutputType]]. Stage boundary data types are derived from [[inDataType]] and
 * [[growEnable]]: boundary `i` is stage `i` input, and boundary `i + 1` is stage `i` output.
 *
 * This design supports FixedPoint data types only.
 *
 * @param fftSize            Number of FFT points. Must be a power of two.
 * @param twiddleType        Complex data type used for twiddle coefficients.
 * @param inDataType         Complex input stream data type.
 * @param decimation         SDF decimation mode.
 * @param sdfRadix           SDF radix implementation selector.
 * @param growEnable         Per-stage growth controls. Each enabled stage grows by one FixedPoint width bit.
 * @param runTime            Enables runtime-selectable FFT size through `i_size`.
 * @param divBy2             Per-stage static divide-by-two controls.
 * @param divBy2Reg          Enables runtime-selectable per-stage divide-by-two controls.
 * @param overflowReg        Enables per-stage overflow status outputs.
 * @param trimType           Default trim mode used when per-stage trim arrays are not supplied.
 * @param numAddPipes        Number of pipeline registers after add/subtract operations.
 * @param numMulPipes        Number of pipeline registers after multiplication operations.
 * @param direction          Static transform direction. `true` selects FFT ordering, `false` selects IFFT ordering.
 * @param directionReg       Enables runtime-selectable transform direction through `i_fft_or_ifft`.
 * @param dspMul4            Uses the four-real-multiplier complex multiplier implementation.
 * @param useBitReverse      Inserts top-level bit reversal for natural-order streaming at both ends.
 * @param drainOnLastReg     Enables an optional input that zero-drains one finite frame after `i_last`.
 * @param minSRAMdepth       Delay lines deeper than this threshold use SRAM-backed storage.
 * @param singlePortSRAM     Uses single-port SRAM for eligible delay lines and bit-reversal buffers.
 * @param stageTrimTypes     Optional per-stage trim mode for SDF butterfly scaling.
 * @param twiddleTrimTypes   Optional per-stage trim mode for twiddle multipliers.
 */
case class FFTParams(
  fftSize:           Int = 1024,
  twiddleType:       DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
  inDataType:        DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
  decimation:        DecimationType = DIF,
  sdfRadix:          SDFRadix = Radix22,
  growEnable:        Seq[Boolean] = Seq.empty,
  runTime:           Boolean = false,
  divBy2:            Seq[Boolean] = Seq.empty,
  divBy2Reg:         Boolean = false,
  overflowReg:       Boolean = false,
  trimType:          TrimType = RoundHalfUp,
  numAddPipes:       Int = 0,
  numMulPipes:       Int = 0,
  direction:         Boolean = true,
  directionReg:      Boolean = false,
  dspMul4:           Boolean = false,
  useBitReverse:     Boolean = false,
  drainOnLastReg:    Boolean = false,
  minSRAMdepth:      Int = 0,
  singlePortSRAM:    Boolean = false,
  stageTrimTypes:    Seq[TrimType] = Seq.empty,
  twiddleTrimTypes:  Seq[TrimType] = Seq.empty) {

  require(isPow2(fftSize), "number of points must be a power of 2")

  private[fft] val stageCount = log2Up(fftSize)
  private[fft] val complexMulLatency =
    if (dspMul4) numAddPipes + numMulPipes else 2 * numAddPipes + numMulPipes
  private[fft] val stageLatency = numAddPipes + complexMulLatency

  private[fft] val stageGrowEnable: IndexedSeq[Boolean] =
    perStage("growEnable", growEnable, false)
  private[fft] val stageDivBy2: IndexedSeq[Boolean] =
    perStage("divBy2", divBy2, true)

  val resolvedStageTrimTypes: IndexedSeq[TrimType] =
    perStage("stageTrimTypes", stageTrimTypes, trimType)
  val resolvedTwiddleTrimTypes: IndexedSeq[TrimType] =
    perStage("twiddleTrimTypes", twiddleTrimTypes, trimType)

  require(inDataType.real.widthKnown, "inDataType FixedPoint width must be known")
  require(twiddleType.real.widthKnown, "twiddleType FixedPoint width must be known")

  require(numAddPipes >= 0, s"numAddPipes must be non-negative, got $numAddPipes")
  require(numMulPipes >= 0, s"numMulPipes must be non-negative, got $numMulPipes")
  require(minSRAMdepth >= 0, s"minSRAMdepth must be non-negative, got $minSRAMdepth")

  private[fft] val stageDataTypes: IndexedSeq[DspComplex[FixedPoint]] =
    stageGrowEnable.scanLeft(inDataType) { case (dataType, grow) =>
      if (grow) growType(dataType) else dataType
    }

  private[fft] def stageInputType(stage: Int): DspComplex[FixedPoint] = {
    requireStageIndex(stage)
    stageDataTypes(stage)
  }

  private[fft] def stageOutputType(stage: Int): DspComplex[FixedPoint] = {
    requireStageIndex(stage)
    stageDataTypes(stage + 1)
  }

  private[fft] def fftOutputType: DspComplex[FixedPoint] = stageDataTypes.last

  private[fft] def radixParams(stage: Int, delay: Int): RadixParams =
    RadixParams(
      inDataType    = stageInputType(stage),
      outDataType   = stageOutputType(stage),
      twiddleType   = twiddleType,
      stageSize     = delay << 1,
      decimation    = decimation,
      sdfRadix      = sdfRadix,
      overflowReg   = overflowReg,
      divBy2Reg     = divBy2Reg,
      divBy2        = stageDivBy2(stage),
      growEnable    = stageGrowEnable(stage),
      latency       = complexMulLatency,
      addPipeRegs   = numAddPipes,
      mulPipeRegs   = numMulPipes,
      dspMul4       = dspMul4,
      delay         = delay,
      bufferAsMem   = minSRAMdepth < delay,
      singlePortMem = singlePortSRAM,
      trimType      = resolvedStageTrimTypes(stage),
    )

  private def perStage[A](name: String, values: Seq[A], default: A): IndexedSeq[A] = {
    val resolved = if (values.isEmpty) Seq.fill(stageCount)(default) else values
    require(
      resolved.length == stageCount,
      s"$name must contain one entry per FFT stage ($stageCount entries for fftSize=$fftSize), got ${resolved.length}"
    )
    resolved.toIndexedSeq
  }

  private def requireStageIndex(stage: Int): Unit =
    require(stage >= 0 && stage < stageCount, s"stage index must be in [0, $stageCount), got $stage")

  private def growType(dataType: DspComplex[FixedPoint]): DspComplex[FixedPoint] =
    DspComplex(FixedPoint((dataType.real.getWidth + 1).W, dataType.real.binaryPoint.get.BP))
}
