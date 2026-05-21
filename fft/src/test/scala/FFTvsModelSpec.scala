package opera.fft

import chisel3.util.log2Up
import chiseltest.ChiselScalatestTester
import chiseltest.iotesters.PeekPokeTester
import dsptools._
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Compares direct RTL FFT cores against the bit-accurate Scala FFT model.
 *
 * This spec instantiates the hardware cores (`R2FFT` and `R22FFT`) as DUTs and
 * uses `FFTModel` only to compute the expected raw output samples. The current
 * scenarios phase-shift the selected FFT tone pattern across frames so a core
 * that drops the first valid output frame will return the wrong frame and fail
 * the raw-bit comparison.
 */
class FFTvsModelSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of "FFT RTL core vs model"

  /**
   * One direct-core DUT-vs-model scenario.
   *
   * @param radix      SDF core implementation to instantiate.
   * @param decimation FFT decimation mode for the core.
   * @param size       Static FFT size used by both DUT and model.
   * @param pattern    Natural-order frame pattern checked against the model.
   */
  private case class FFTvsModelCase(radix: SDFRadix, decimation: DecimationType, size: Int, pattern: InputPatterns.FftFramePattern)
  private case class FFTTrimCase(radix: SDFRadix, decimation: DecimationType, size: Int, trimName: String, trimType: TrimType)
  private case class FFTStallCase(radix: SDFRadix, decimation: DecimationType, size: Int)

  private val dutAmplitudeRaw = BigInt(256)
  private val dutNoiseAmplitudeRaw = 32

  private val decimations  = Seq(DIF, DIT)
  private val radix2Sizes  = Seq(2, 4, 8, 64, 256, 1024)
  private val radix22Sizes = Seq(4, 16, 64, 256, 1024)

  private val baseConfigs = radix2Sizes.flatMap(size => decimations.map(decimation => (Radix2, decimation, size))) ++
                            radix22Sizes.flatMap(size => decimations.map(decimation => (Radix22, decimation, size)))

  private val configs = baseConfigs.flatMap { case (radix, decimation, size) =>
    InputPatterns
      .standardFftPatterns(
        size,
        dutAmplitudeRaw,
        noiseSeed         = 0xC0FFEEL + size + (if (decimation == DIT) 1 else 0),
        noiseAmplitudeRaw = dutNoiseAmplitudeRaw
      )
      .map(pattern => FFTvsModelCase(radix, decimation, size, pattern))
  }

  private val trimTypeSeq: Seq[(String, TrimType)] = Seq(
    "Floor"                    -> Floor,
    "Ceiling"                  -> Ceiling,
    "Convergent"               -> Convergent,
    "Round"                    -> Round,
    "RoundDown"                -> RoundDown,
    "RoundUp"                  -> RoundUp,
    "RoundTowardsZero"         -> RoundTowardsZero,
    "RoundTowardsInfinity"     -> RoundTowardsInfinity,
    "RoundHalfDown"            -> RoundHalfDown,
    "RoundHalfUp"              -> RoundHalfUp,
    "RoundHalfTowardsZero"     -> RoundHalfTowardsZero,
    "RoundHalfTowardsInfinity" -> RoundHalfTowardsInfinity,
    "RoundHalfToEven"          -> RoundHalfToEven,
    "RoundHalfToOdd"           -> RoundHalfToOdd,
  )

  private val trimConfigs =
    for {
      (trimName, trimType) <- trimTypeSeq
      radix                <- Seq(Radix2, Radix22)
      decimation           <- decimations
    } yield FFTTrimCase(radix, decimation, size = 64, trimName, trimType)

  private val stallConfigs =
    for {
      radix      <- Seq(Radix2, Radix22)
      decimation <- decimations
    } yield FFTStallCase(radix, decimation, size = 64)

  private def directCore(radix: SDFRadix, params: FFTParams): HasIO =
    radix match {
      case Radix2  => new R2FFT(params)
      case Radix22 => new R22FFT(params)
    }

  private def runDirectTest(radix: SDFRadix, params: FFTParams)(tester: HasIO => PeekPokeTester[HasIO]): Unit =
    test(directCore(radix, params))
      .withAnnotations(TestConfig.annotationsForFftSize(params.fftSize))
      .runPeekPoke(tester)

  configs.foreach { config =>
    it should TestUtils.passWhen(
      "check"      -> "direct core emits first model frame without loss",
      "radix"      -> config.radix.label,
      "decimation" -> config.decimation,
      "size"       -> config.size,
      "pattern"    -> config.pattern.label,
    ) in {
      val params   = FFTModelTestUtils.fftParams(config.radix, config.size, config.decimation)
      val input    = FFTModelTestUtils.patternedDutInput(params, config.pattern, frames = 3)
      val expected = FFTModel(params, input).checkedFrame(params.fftSize)
      val plotName = s"dut-vs-model-${config.radix.label}-${config.decimation}-${config.size}-${config.pattern.label}"
      runDirectTest(config.radix, params)(dut => new FFTvsModelTester(dut, params, input, expected, plotName))
    }
  }

  trimConfigs.foreach { config =>
    it should TestUtils.passWhen(
      "check"      -> "direct core trim rounding matches model",
      "radix"      -> config.radix.label,
      "decimation" -> config.decimation,
      "size"       -> config.size,
      "trimType"   -> config.trimName,
    ) in {
      val stages = log2Up(config.size)
      val params = FFTModelTestUtils
        .fftParams(config.radix, config.size, config.decimation)
        .copy(
          trimType         = config.trimType,
          stageTrimTypes   = Seq.fill(stages)(config.trimType),
          twiddleTrimTypes = Seq.fill(stages)(config.trimType)
        )
      val pattern = InputPatterns.multiTonePattern(
        params.fftSize,
        dutAmplitudeRaw,
        noise = Some(InputPatterns.FftNoise(dutNoiseAmplitudeRaw, seed = 0x51A7E000L + config.trimName.hashCode.abs)),
        label = s"trim-${config.trimName}"
      )
      val input    = FFTModelTestUtils.patternedDutInput(params, pattern, frames = 3)
      val expected = FFTModel(params, input).checkedFrame(params.fftSize)
      val plotName = s"dut-vs-model-trim-${config.trimName}-${config.radix.label}-${config.decimation}-${config.size}"
      runDirectTest(config.radix, params)(dut => new FFTvsModelTester(dut, params, input, expected, plotName))
    }
  }

  stallConfigs.foreach { config =>
    it should TestUtils.passWhen(
      "check"      -> "output stall backpressures direct core input without loss",
      "radix"      -> config.radix.label,
      "decimation" -> config.decimation,
      "size"       -> config.size,
    ) in {
      val params  = FFTModelTestUtils.fftParams(config.radix, config.size, config.decimation)
      val pattern = InputPatterns.multiTonePattern(
        params.fftSize,
        dutAmplitudeRaw,
        noise = Some(InputPatterns.FftNoise(dutNoiseAmplitudeRaw, seed = 0x5A11C0DEL + config.size + config.radix.label.hashCode.abs)),
        label = "stall"
      )
      val input    = FFTModelTestUtils.patternedDutInput(params, pattern, frames = 4)
      val expected = FFTModel(params, input).checkedFrame(params.fftSize)
      val label    = s"stall-${config.radix.label}-${config.decimation}-${config.size}"
      runDirectTest(config.radix, params)(dut => new FFTOutputStallTester(dut, params, input, expected, label))
    }
  }
}
