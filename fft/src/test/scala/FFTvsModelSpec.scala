package opera.fft

import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import ModelUtils.RawComplex

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

  /**
   * Builds deterministic raw DUT input for a direct FFT core.
   *
   * `FFTModel` and the direct RTL cores both operate in native SDF order. For DIT,
   * the core input is therefore frame-wise bit-reversed before it is driven into
   * the DUT. Tone phases and noise seeds are shifted per frame so repeated test
   * frames remain deterministic but not identical.
   *
   * @param params  FFT parameters shared by DUT and model.
   * @param pattern Natural-order frame pattern to generate.
   * @param frames  Number of complete FFT frames to generate.
   */
  private def dutInput(params: FFTParams, pattern: InputPatterns.FftFramePattern, frames: Int): Vector[RawComplex] =
    Vector
      .tabulate(frames)(frameIndex => InputPatterns.fftFrame(params, framePattern(pattern, frameIndex)))
      .flatMap { frame =>
        if (params.decimation == DIT) BitReverseUtils.bitReverse(frame.toVector) else frame
      }
      .toVector

  private def framePattern(pattern: InputPatterns.FftFramePattern, frameIndex: Int): InputPatterns.FftFramePattern =
    pattern.copy(
      tones = pattern.tones.map(tone => tone.copy(phaseRadians = tone.phaseRadians + frameIndex.toDouble * 0.37)),
      noise = pattern.noise.map(noise => noise.copy(seed = noise.seed + frameIndex.toLong))
    )

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
      val input = dutInput(params, config.pattern, frames = 3)
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
