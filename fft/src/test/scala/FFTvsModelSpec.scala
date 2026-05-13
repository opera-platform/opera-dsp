package opera.fft

import chiseltest.ChiselScalatestTester
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

  private val dutAmplitudeRaw = BigInt(256)
  private val dutNoiseAmplitudeRaw = 32

  private val decimations = Seq(DIF, DIT)
  private val radix2Sizes = Seq(2, 4, 8, 64, 256, 1024)
  private val radix22Sizes = Seq(4, 16, 64, 256, 1024)

  private val baseConfigs =
    radix2Sizes.flatMap(size => decimations.map(decimation => (Radix2, decimation, size))) ++
      radix22Sizes.flatMap(size => decimations.map(decimation => (Radix22, decimation, size)))

  private val configs = baseConfigs.flatMap { case (radix, decimation, size) =>
    InputPatterns
      .standardFftPatterns(
        size,
        dutAmplitudeRaw,
        noiseSeed         = 0x51dfL + size + (if (decimation == DIT) 1 else 0),
        noiseAmplitudeRaw = dutNoiseAmplitudeRaw
      )
      .map(pattern => FFTvsModelCase(radix, decimation, size, pattern))
  }

  private def annotationsFor(size: Int) = if (size >= 256) TestConfig.nonParallelVerilatorAnnotations else TestConfig.annotations

  configs.foreach { config =>
    it should TestUtils.passWhen(
      "check"      -> "first output frame is not dropped",
      "radix"      -> config.radix.label,
      "decimation" -> config.decimation,
      "size"       -> config.size,
      "pattern"    -> config.pattern.label,
    ) in {
      val params = FFTModelTestUtils.fftParams(config.radix, config.size, config.decimation)
      val input = FFTModelTestUtils.patternedDutInput(params, config.pattern, frames = 3)
      val expected = FFTModel(params, input).checkedFrame(params.fftSize)
      val plotName = s"dut-vs-model-${config.radix.label}-${config.decimation}-${config.size}-${config.pattern.label}"
      val testAnnotations = annotationsFor(config.size)

      config.radix match {
        case Radix2 =>
          test(new R2FFT(params))
            .withAnnotations(testAnnotations)
            .runPeekPoke(dut => new FFTvsModelTester(dut, params, input, expected, plotName))
        case Radix22 =>
          test(new R22FFT(params))
            .withAnnotations(testAnnotations)
            .runPeekPoke(dut => new FFTvsModelTester(dut, params, input, expected, plotName))
      }
    }
  }
}
