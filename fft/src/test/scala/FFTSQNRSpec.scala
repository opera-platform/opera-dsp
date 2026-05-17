package opera.fft

import chisel3.util.log2Up
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Characterizes public FFT DUT SQNR against a floating-point FFT reference.
 */
class FFTSQNRSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of "FFT DUT SQNR"

  private case class SQNRCase(
      check       : String,
      radix       : SDFRadix,
      decimation  : DecimationType,
      size        : Int,
      dataWidth   : Int = 16,
      binPoint    : Int = 14,
      twiddleWidth: Int = 16,
      growName    : String = "default divide-by-2",
      growEnable  : Seq[Boolean] = Seq.empty,
  ) {
    def params: FFTParams =
      FFTModelTestUtils.fftParams(radix, size, decimation, dataWidth = dataWidth, binPoint = binPoint,
        twiddleWidth = twiddleWidth, growEnable = growEnable)

    def plotName: String =
      Seq("fft-sqnr-fmcw-target-chirp", radix.label, decimation, size, s"$dataWidth.$binPoint", growName)
        .mkString("-")

    def seed: Long =
      0xC0FFEEL +
        (if (radix == Radix22) 0x2200L else 0x2000L) +
        (if (decimation == DIT) 0x100L else 0L) +
        size.toLong +
        dataWidth.toLong +
        growName.hashCode.toLong
  }

  private val radixSeq = Seq(Radix2, Radix22)
  private val decimationSeq = Seq(DIF, DIT)
  private val sqnrSizeSeq = Seq(64, 1024)
  private val precisionFormatSeq = Seq((16, 14, 16), (20, 18, 20), (24, 22, 24))

  private def stageGrowthPatternSeq(stages: Int): Seq[(String, Seq[Boolean])] =
    Seq(
      "all stages grow" -> Seq.fill(stages)(true),
      "first and last stages grow" -> Seq.tabulate(stages)(stage => stage == 0 || stage == stages - 1),
      "even stages grow" -> Seq.tabulate(stages)(stage => stage % 2 == 0),
      "odd stages grow" -> Seq.tabulate(stages)(stage => stage % 2 == 1)
    )

  private val precisionCases = for {
    size <- sqnrSizeSeq
    radix <- radixSeq
    decimation <- decimationSeq
    (dataWidth, binPoint, twiddleWidth) <- precisionFormatSeq
  } yield SQNRCase(
    check        = "FFT fixed-point precision",
    radix        = radix,
    decimation   = decimation,
    size         = size,
    dataWidth    = dataWidth,
    binPoint     = binPoint,
    twiddleWidth = twiddleWidth
  )

  private val growthCases = for {
    size <- sqnrSizeSeq
    radix <- radixSeq
    decimation <- decimationSeq
    (name, growEnable) <- stageGrowthPatternSeq(log2Up(size))
  } yield SQNRCase(
    check        = "FFT stage growth",
    radix        = radix,
    decimation   = decimation,
    size         = size,
    growName     = name,
    growEnable   = growEnable
  )

  (precisionCases ++ growthCases).foreach { config =>
    it should TestUtils.passWhen(
      "check" -> config.check,
      "radix" -> config.radix.label,
      "decimation" -> config.decimation,
      "size" -> config.size,
      "format" -> s"${config.dataWidth}.${config.binPoint}",
      "growth" -> config.growName,
    ) in {
      val params = config.params
      val pattern = noisyMultiTonePattern(params, config)

      test(new FFT(params))
        .withAnnotations(TestConfig.annotationsForFftSize(config.size))
        .runPeekPoke(dut =>
          new FFTSQNRTester(
            dut      = dut,
            params   = params,
            pattern  = pattern,
            plotName = config.plotName
          )
        )
    }
  }

  private def noisyMultiTonePattern(params: FFTParams, config: SQNRCase): InputPatterns.FftFramePattern = {
    val amplitude = safeAmplitudeRaw(params)
    val noiseAmplitude = math.max(1, (amplitude / 32).min(BigInt(Int.MaxValue)).toInt)
    InputPatterns.multiTonePattern(
      size             = params.fftSize,
      baseAmplitudeRaw = amplitude,
      noise            = Some(InputPatterns.FftNoise(noiseAmplitude, config.seed)),
      label            = "fft-sqnr-fmcw-multi-tone-noise"
    )
  }

  private def safeAmplitudeRaw(params: FFTParams): BigInt = {
    val format = FFTModel.inputFormat(params)
    val exponent = math.max(1, math.min(format.binaryPoint - 4, format.width - 5))
    BigInt(1) << exponent
  }
}
