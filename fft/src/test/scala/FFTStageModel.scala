package opera.fft

import dsptools._
import dsptools.numbers.DspComplex
import fixedpoint.{FixedPoint, KnownBinaryPoint}

object FFTStageModel {
  /**
   * FixedPoint format used by the pure Scala SDF models.
   * Model supports only FixedPoint.
   *
   * @param width       Signed raw storage width in bits.
   * @param binaryPoint Number of fractional bits in the raw value.
   */
  final case class FixedFormat(width: Int, binaryPoint: Int) {
    require(width > 1, "fixed-point width must include sign and data bits")
    require(binaryPoint >= 0, "binary point must be non-negative")

    val minRaw: BigInt = -(BigInt(1) << (width - 1))
    val maxRaw: BigInt = (BigInt(1) << (width - 1)) - 1

    private val modulus = BigInt(1) << width
    private val signBit = BigInt(1) << (width - 1)

    def fits(raw: BigInt): Boolean = raw >= minRaw && raw <= maxRaw

    def wrap(raw: BigInt): BigInt = {
      val masked = raw & (modulus - 1)
      if (masked >= signBit) masked - modulus else masked
    }

    def toDouble(raw: BigInt): Double = raw.toDouble / math.pow(2.0, binaryPoint.toDouble)
  }

  object FixedFormat {
    def from(dataType: DspComplex[FixedPoint]): FixedFormat = {
      dataType.real match {
        case fixed: FixedPoint =>
          require(fixed.widthKnown, "FFTStageModel requires known FixedPoint width")
          fixed.binaryPoint match {
            case KnownBinaryPoint(binaryPoint) => FixedFormat(fixed.getWidth, binaryPoint)
            case other => throw new IllegalArgumentException(s"FFTStageModel requires known binary point, got $other")
          }
        case other => throw new IllegalArgumentException(s"FFTStageModel supports FixedPoint only, got ${other.getClass.getName}")
      }
    }

  }

  /**
   * Raw signed integer representation of one complex sample.
   *
   * @param real Raw FixedPoint bits for the real component.
   * @param imag Raw FixedPoint bits for the imaginary component.
   */
  final case class RawComplex(real: BigInt, imag: BigInt) {
    def +(that: RawComplex): RawComplex = RawComplex(real + that.real, imag + that.imag)
    def -(that: RawComplex): RawComplex = RawComplex(real - that.real, imag - that.imag)
    def map(f: BigInt => BigInt): RawComplex = RawComplex(f(real), f(imag))
  }

  /**
   * Internal butterfly state captured for one SDF model cycle.
   *
   * @param input          Input sample after wrapping to the stage format.
   * @param delayOperand   Sample read from the feedback delay path.
   * @param sum            Unwrapped butterfly sum.
   * @param diff           Unwrapped butterfly difference.
   * @param scaledSum      Sum after grow/divide-by-two handling and wrapping.
   * @param scaledDiff     Difference after grow/divide-by-two handling and wrapping.
   * @param delayInput     Sample written back into the delay path.
   * @param selectedOutput Sample selected before the output pipeline.
   * @param output         Output sample after the modeled output pipeline.
   * @param valid          Output valid flag for this cycle.
   * @param counter        Stage counter value used for this cycle.
   * @param overflow       Indicates that unscaled butterfly math exceeded the stage format.
   */
  final case class StageTrace(
      input:          RawComplex,
      delayOperand:   RawComplex,
      sum:            RawComplex,
      diff:           RawComplex,
      scaledSum:      RawComplex,
      scaledDiff:     RawComplex,
      delayInput:     RawComplex,
      selectedOutput: RawComplex,
      output:         RawComplex,
      valid:          Boolean,
      counter:        Int,
      overflow:       Boolean,
  )

  /**
   * Result returned by one SDF model step.
   *
   * @param output   Output sample for this cycle.
   * @param valid    Output valid flag for this cycle.
   * @param counter  Stage counter value used for this cycle.
   * @param overflow Indicates that unscaled butterfly math exceeded the stage format.
   * @param trace    Detailed internal butterfly trace for debugging tests.
   */
  final case class CycleResult(
      output  : RawComplex,
      valid   : Boolean,
      counter : Int,
      overflow: Boolean,
      trace   : StageTrace,
  )

  /**
   * Divides a raw value by two using round-half-to-even behavior.
   *
   * @param raw Signed raw integer value.
   */
  def roundHalfToEvenDiv2(raw: BigInt): BigInt = {
    if ((raw & 1) == 0) raw / 2
    else {
      val lower = floorDiv2(raw)
      if ((lower & 1) == 0) lower else lower + 1
    }
  }

  /**
   * Divides a raw value by two using round-half-to-odd behavior.
   *
   * @param raw Signed raw integer value.
   */
  def roundHalfToOddDiv2(raw: BigInt): BigInt = {
    if ((raw & 1) == 0) raw / 2
    else {
      val lower = floorDiv2(raw)
      if ((lower & 1) == 1) lower else lower + 1
    }
  }

  private[fft] def floorDiv2(raw: BigInt): BigInt = if (raw >= 0) raw / 2 else -(((-raw) + 1) / 2)

  private[fft] def ceilDiv2(raw: BigInt): BigInt = -floorDiv2(-raw)

  private final class Pipe[A](depth: Int, init: A) {
    private val initialValues = Vector.fill(depth)(init)
    private var values = initialValues

    def out(input: A): A = if (depth == 0) input else values.last
    def shift(input: A): Unit = if (depth > 0) values = input +: values.dropRight(1)
    def reset(): Unit = values = initialValues
  }
}

/**
 * Pure Scala bit-accurate model of one SDF radix stage.
 *
 * @param params Radix stage parameters mirrored from the corresponding hardware module.
 */
class FFTStageModel(val params: RadixParams, counterInitOverride: Option[Int] = None) {
  import FFTStageModel._

  require(params.delay == params.stageSize / 2, s"FFTStageModel expects delay = stageSize / 2, got delay=${params.delay}, stageSize=${params.stageSize}")

  val inputFormat: FixedFormat = FixedFormat.from(params.inDataType)
  val outputFormat: FixedFormat = FixedFormat.from(params.outDataType)
  val format: FixedFormat = inputFormat

  val delayLength: Int = params.delay
  val counterMask: Int = params.stageSize - 1
  val counterInit: Int = counterInitOverride.getOrElse(0)
  val controlLatency: Int = if (params.decimation == DIF) 0 else params.latency
  private val zero = RawComplex(0, 0)

  private val delayEnablePipe = new Pipe[Boolean](controlLatency, init = false)
  private val controlPipe = new Pipe[Boolean](controlLatency, init = false)
  private val outputPipe = new Pipe[RawComplex](params.addPipeRegs, zero)
  private val outputValidLatency =
    if (params.decimation == DIF) params.latency + params.addPipeRegs else params.addPipeRegs
  private val validPipe = new Pipe[Boolean](outputValidLatency, init = false)

  private var delayValues = Vector.fill(delayLength)(zero)
  private var delayFreshCount = 0
  private var counter = counterInit

  def reset(): Unit = {
    delayEnablePipe.reset()
    controlPipe.reset()
    validPipe.reset()
    delayFreshCount = 0
    counter = counterInit
  }

  /**
   * Advances only the unreset data path for one cycle while the stage reset is asserted.
   *
   * Hardware reset clears the control registers, but the SDF delay storage and output
   * data pipe dont have reset signals. Tests call this to keep the model aligned
   * with reset cycles that still clock data registers.
   */
  def stepReset(input: RawComplex = zero, divBy2: Boolean = params.divBy2): CycleResult = {
    delayEnablePipe.reset()
    controlPipe.reset()
    validPipe.reset()
    delayFreshCount = 0
    counter = counterInit

    val stageInput = input.map(inputFormat.wrap)
    val delayOut = delayValues.last.map(outputFormat.wrap)
    val delayOperand = delayOut.map(inputFormat.wrap)
    val sum = delayOperand + stageInput
    val diff = delayOperand - stageInput
    val overflow = !params.growEnable && Seq(sum.real, sum.imag, diff.real, diff.imag).exists(raw => !inputFormat.fits(raw))
    val scaledSum = scaleButterflyOutput(sum, divBy2)
    val scaledDiff = scaleButterflyOutput(diff, divBy2)
    val beforePipe = scaledSum
    val output = outputPipe.out(beforePipe)
    outputPipe.shift(beforePipe)
    val visibleOverflow = false

    CycleResult(
      output = output,
      valid = false,
      counter = counterInit,
      overflow = visibleOverflow,
      trace = StageTrace(
        input          = stageInput,
        delayOperand   = delayOperand,
        sum            = sum,
        diff           = diff,
        scaledSum      = scaledSum,
        scaledDiff     = scaledDiff,
        delayInput     = scaledDiff,
        selectedOutput = beforePipe,
        output         = output,
        valid          = false,
        counter        = counterInit,
        overflow       = visibleOverflow,
      )
    )
  }

  def currentCounter: Int = counter

  /**
   * Advances the model by one enabled or idle cycle.
   *
   * @param input  Input sample for this cycle.
   * @param enable If `true`, advances the stage counter and valid pipeline.
   * @param divBy2 Per-cycle divide-by-two control used when growth is disabled.
   */
  def step(input: RawComplex, enable: Boolean, divBy2: Boolean): CycleResult = {
    val counterBefore = counter
    val inFirstHalf = (counterBefore & delayLength) == 0
    val delayEnable = delayEnablePipe.out(enable)
    val delayFresh = delayFreshCount == delayLength
    val muxControl = controlPipe.out(inFirstHalf)
    val stageInput = input.map(inputFormat.wrap)
    val delayOut = delayValues.last.map(outputFormat.wrap)
    val delayOperand = delayOut.map(inputFormat.wrap)
    val sum = delayOperand + stageInput
    val diff = delayOperand - stageInput
    val overflow = !params.growEnable && Seq(sum.real, sum.imag, diff.real, diff.imag).exists(raw => !inputFormat.fits(raw))
    val scaledSum = scaleButterflyOutput(sum, divBy2)
    val scaledDiff = scaleButterflyOutput(diff, divBy2)
    val delayIn = if (muxControl) stageInput.map(outputFormat.wrap) else scaledDiff
    val beforePipe = if (muxControl) delayOut else scaledSum
    val output = outputPipe.out(beforePipe)
    val delayFillEnable = if (params.decimation == DIF) enable else delayEnable
    val outputValidBeforePipe = delayFresh && (if (params.decimation == DIF) enable else delayEnable)
    val valid = validPipe.out(outputValidBeforePipe)
    val visibleOverflow = overflow && outputValidBeforePipe
    val trace = StageTrace(
      input          = stageInput,
      delayOperand   = delayOperand,
      sum            = sum,
      diff           = diff,
      scaledSum      = scaledSum,
      scaledDiff     = scaledDiff,
      delayInput     = delayIn,
      selectedOutput = beforePipe,
      output         = output,
      valid          = valid,
      counter        = counterBefore,
      overflow       = visibleOverflow,
    )

    counter = (counterBefore + (if (enable) 1 else 0)) & counterMask
    delayEnablePipe.shift(enable)
    controlPipe.shift(inFirstHalf)
    if (delayEnable) delayValues = delayIn +: delayValues.dropRight(1)
    if (delayFillEnable && !delayFresh) delayFreshCount += 1

    outputPipe.shift(beforePipe)
    validPipe.shift(outputValidBeforePipe)

    CycleResult(output, valid, counterBefore, visibleOverflow, trace)
  }

  private[fft] def scaleButterflyOutput(value: RawComplex, divBy2: Boolean): RawComplex = {
    val raw = if (params.growEnable) {
      value
    } else if (divBy2) {
      value.map(div2)
    } else {
      value
    }
    raw.map(outputFormat.wrap)
  }

  private def div2(raw: BigInt): BigInt =
    params.trimType match {
      case RoundDown                 => FFTStageModel.floorDiv2(raw)
      case RoundUp                   => FFTStageModel.ceilDiv2(raw)
      case RoundTowardsZero          => raw / 2
      case RoundTowardsInfinity      => if (raw >= 0) FFTStageModel.ceilDiv2(raw) else FFTStageModel.floorDiv2(raw)
      case RoundHalfDown             => if ((raw & 1) == 0) raw / 2 else FFTStageModel.floorDiv2(raw)
      case RoundHalfUp               => if ((raw & 1) == 0) raw / 2 else FFTStageModel.floorDiv2(raw) + 1
      case RoundHalfTowardsZero      => if ((raw & 1) == 0) raw / 2 else if (raw >= 0) FFTStageModel.floorDiv2(raw) else FFTStageModel.ceilDiv2(raw)
      case RoundHalfTowardsInfinity  => if ((raw & 1) == 0) raw / 2 else if (raw >= 0) FFTStageModel.ceilDiv2(raw) else FFTStageModel.floorDiv2(raw)
      case RoundHalfToEven           => FFTStageModel.roundHalfToEvenDiv2(raw)
      case RoundHalfToOdd            => FFTStageModel.roundHalfToOddDiv2(raw)
      case NoTrim                    => throw new IllegalArgumentException("FFTStageModel does not support NoTrim for div2")
      case trimType: TrimType =>
        throw new IllegalArgumentException(s"FFTStageModel does not support trim type $trimType")
    }
}
