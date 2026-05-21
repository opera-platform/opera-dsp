package opera.fft

import chisel3.util.log2Up
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Compares the bit-accurate FFT model against a floating-point FFT reference.
 */
class FFTModelvsFloatingPointSpec extends AnyFlatSpec with TestConfigSupport {
  behavior of "FFTModel vs FloatingPoint"

  private val radixSeq                  = Seq(Radix2, Radix22)
  private val decimationSeq             = Seq(DIF, DIT)
  private val r2ModelSizeSeq            = Seq(4, 8, 16, 64, 256, 1024)
  private val r22ModelSizeSeq           = Seq(4, 16, 64, 256, 1024)
  private val r2HighPrecisionSizeSeq    = Seq(1024, 2048)
  private val r22HighPrecisionSizeSeq   = Seq(1024)
  private val highPrecisionAmplitudeRaw = BigInt(1) << 18

  private def modelSizes(radix: SDFRadix): Seq[Int] =
    if (radix == Radix22) r22ModelSizeSeq else r2ModelSizeSeq

  private def highPrecisionSizes(radix: SDFRadix): Seq[Int] =
    if (radix == Radix22) r22HighPrecisionSizeSeq else r2HighPrecisionSizeSeq

  private def highPrecisionPatterns(size: Int): Seq[InputPatterns.FftFramePattern] =
    InputPatterns.standardFftPatterns(
      size,
      highPrecisionAmplitudeRaw,
      noiseSeed = 0xC0FFEEL + size
    )

  // Checks model ordering and per-stage scaling by comparing deterministic frames against an independent floating-point FFT.
  {
    val configs: Iterator[FFTModelTestUtils.ModelComparisonConfiguration] = for {
      radix      <- radixSeq.iterator
      decimation <- decimationSeq.iterator
      size       <- modelSizes(radix).iterator
    } yield FFTModelTestUtils.ModelComparisonConfiguration(radix, decimation, size)

    configs.foreach { configuration =>
      it should TestUtils.passWhen(
        "check"      -> "match floating-point FFT within fixed-point tolerance",
        "radix"      -> configuration.radix.label,
        "decimation" -> configuration.decimation,
        "size"       -> configuration.size
      ) in {
        val params = FFTModelTestUtils.fftParams(configuration.radix, configuration.size, configuration.decimation)
        val frame  = FFTModelTestUtils.deterministicInput(
          params,
          frames = 1,
          seed   = 0xC0FFEEL + configuration.size + (if (configuration.decimation == DIT) 1 else 0)
        ).take(configuration.size)
        val outFormat = FFTModel.stageFormat(params, log2Up(configuration.size) - 1)
        val tol       = math.max(0.02, 128.0 * math.pow(2.0, -outFormat.binaryPoint.toDouble))
        FFTModelTestUtils
          .compareModelToFloatingPoint(
            params,
            frame,
            tol,
            s"model-vs-floating-point-${configuration.radix.label}-${configuration.decimation}-${configuration.size}"
          )
          .foreach(file => info(s"wrote model-vs-floating-point FFT plot to ${file.getAbsolutePath}"))
      }
    }
  }

  // Checks numerical precision by comparing larger 32-bit fixed-point patterns against FloatingPoint with tight tolerance.
  {
    val configs: Iterator[FFTModelTestUtils.HighPrecisionConfiguration] = for {
      radix      <- radixSeq.iterator
      decimation <- decimationSeq.iterator
      size       <- highPrecisionSizes(radix).iterator
      pattern    <- highPrecisionPatterns(size).iterator
    } yield FFTModelTestUtils.HighPrecisionConfiguration(radix, decimation, size, pattern)

    configs.foreach { configuration =>
      it should TestUtils.passWhen(
        "check"      -> "match floating-point FFT tightly with high-precision FixedPoint",
        "radix"      -> configuration.radix.label,
        "decimation" -> configuration.decimation,
        "size"       -> configuration.size,
        "pattern"    -> configuration.pattern.label
      ) in {
        val params = FFTModelTestUtils.fftParams(
          radix        = configuration.radix,
          size         = configuration.size,
          decimation   = configuration.decimation,
          dataWidth    = 32,
          binPoint     = 28,
          twiddleWidth = 32
        )
        val frame     = InputPatterns.fftFrame(params, configuration.pattern)
        val outFormat = FFTModel.stageFormat(params, log2Up(configuration.size) - 1)
        val tol       = math.max(1.0e-6, 256.0 * math.pow(2.0, -outFormat.binaryPoint.toDouble))
        FFTModelTestUtils
          .compareModelToFloatingPoint(
            params,
            frame,
            tol,
            s"high-precision-floating-point-${configuration.radix.label}-${configuration.decimation}-${configuration.size}-${configuration.pattern.label}"
          )
          .foreach(file => info(s"wrote model-vs-floating-point FFT plot to ${file.getAbsolutePath}"))
      }
    }
  }
}
