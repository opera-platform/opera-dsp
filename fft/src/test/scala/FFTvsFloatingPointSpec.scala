package opera.fft

import chisel3.util.log2Up
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec

private final case class FFTDutConfiguration(
    size:           Int,
    decimation:     DecimationType,
    addPipeRegs:    Int = 1,
    mulPipeRegs:    Int = 1,
    use4Muls:       Boolean = false,
    minSRAMdepth:   Int = 0,
    singlePortSRAM: Boolean = false,
    growEnable:     Seq[Boolean] = Seq.empty,
)

private final case class FloatingPointCase(
    radix:       SDFRadix,
    check:       FloatingPointCheck,
    fft:         FFTDutConfiguration,
    name:        String = "",
    seedOffset:  Int = 0,
    inputFrames: Int = 2,
    growName:    String = "",
)

private final case class StageGrowthPattern(name: String, seedOffset: Int, grow: Int => Seq[Boolean])

private final case class RadixFloatingPointMatrix(
    radix                : SDFRadix,
    check                : FloatingPointCheck,
    sizes                : Seq[Int],
    multipleFrameSizes   : Seq[Int],
    use4MulsSizes        : Seq[Int],
    minSRAMdepth         : Int,
    multipleFrameSeedBase: Int,
    includeSelectedSignal: Boolean = false,
)

/**
 * Compares the public streaming FFT wrapper against an independent floating-point FFT reference.
 */
class FFTvsFloatingPointSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of "FFT vs FloatingPoint"

  private val decimationSeq: Seq[DecimationType] = Seq(DIF, DIT)

  private val r2SizeSeq: Seq[Int] = Seq(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
  private val r2MultipleFrameCheckSizeSeq: Seq[Int] = Seq(64, 128, 512, 1024)
  private val multipleFrameInputCountSeq: Seq[Int] = Seq(2, 3, 4)
  private val r2Use4MulsSizeSeq: Seq[Int] = Seq(128, 256)
  private val pipeRegsSeq: Seq[(Int, Int)] = Seq((1, 0), (0, 1), (1, 1), (2, 2))
  private val singlePortSRAMSeq: Seq[Boolean] = Seq(true, false)

  private val r22SizeSeq: Seq[Int] = Seq(4, 16, 64, 256, 1024)
  private val r22MultipleFrameCheckSizeSeq: Seq[Int] = Seq(256, 1024)
  private val r22Use4MulsSizeSeq: Seq[Int] = Seq(64, 256)

  private val stageGrowthSizeSeq: Seq[Int] = Seq(64, 1024)
  private val stageGrowthPatternSeq: Seq[StageGrowthPattern] = Seq(
    StageGrowthPattern("all stages grow", seedOffset = 0, stages => Seq.fill(stages)(true)),
    StageGrowthPattern("first and last stages grow", seedOffset = 1, stages => Seq.tabulate(stages)(stage => stage == 0 || stage == stages - 1)),
    StageGrowthPattern("even stages grow", seedOffset = 2, stages => Seq.tabulate(stages)(stage => (stage & 1) == 0)),
    StageGrowthPattern("odd stages grow", seedOffset = 3, stages => Seq.tabulate(stages)(stage => (stage & 1) == 1)),
  )

  private val dutAmplitudeRaw = BigInt(256)
  private val dutNoiseAmplitudeRaw = 32

  private val radixMatrices = Seq(
    RadixFloatingPointMatrix(Radix2, FirstValidOutputFrame, r2SizeSeq, r2MultipleFrameCheckSizeSeq, r2Use4MulsSizeSeq, 8, 80),
    RadixFloatingPointMatrix(Radix22, InitialStoring, r22SizeSeq, r22MultipleFrameCheckSizeSeq, r22Use4MulsSizeSeq, 16, 90, includeSelectedSignal = true),
  )

  private def radixConfigs(matrix: RadixFloatingPointMatrix): Iterator[FloatingPointCase] = {
    val baseline = for {
      size       <- matrix.sizes.iterator
      decimation <- decimationSeq.iterator
    } yield FloatingPointCase(
      radix = matrix.radix,
      check = matrix.check,
      fft   = FFTDutConfiguration(size = size, decimation = decimation),
    )

    val pipeline = for {
      size                       <- matrix.sizes.iterator
      decimation                 <- decimationSeq.iterator
      (addPipeRegs, mulPipeRegs) <- pipeRegsSeq.iterator
    } yield FloatingPointCase(
      radix = matrix.radix,
      check = matrix.check,
      name  = "pipeline variant",
      fft = FFTDutConfiguration(
        size        = size,
        decimation  = decimation,
        addPipeRegs = addPipeRegs,
        mulPipeRegs = mulPipeRegs,
      ),
      seedOffset = 10 + addPipeRegs * 3 + mulPipeRegs,
    )

    val selectedSignal =
      if (matrix.includeSelectedSignal) {
        decimationSeq.iterator.map(decimation =>
          FloatingPointCase(
            radix      = matrix.radix,
            check      = FirstValidOutputFrame,
            name       = "selected non-sparse signal",
            fft        = FFTDutConfiguration(size = 64, decimation = decimation),
            seedOffset = 70,
          )
        )
      } else {
        Iterator.empty
      }

    val multipleFrames = for {
      size        <- matrix.multipleFrameSizes.iterator
      decimation  <- decimationSeq.iterator
      inputFrames <- multipleFrameInputCountSeq.iterator
    } yield FloatingPointCase(
      radix       = matrix.radix,
      check       = MultipleAcceptedFrames,
      fft         = FFTDutConfiguration(size = size, decimation = decimation),
      inputFrames = inputFrames,
      seedOffset  = matrix.multipleFrameSeedBase + inputFrames,
    )

    val fourMul = for {
      size       <- matrix.use4MulsSizes.iterator
      decimation <- decimationSeq.iterator
    } yield FloatingPointCase(
      radix = matrix.radix,
      check = matrix.check,
      name  = "four real multipliers",
      fft   = FFTDutConfiguration(size = size, decimation = decimation, use4Muls = true),
      seedOffset = 50,
    )

    val sram = for {
      decimation     <- decimationSeq.iterator
      singlePortSRAM <- singlePortSRAMSeq.iterator
    } yield FloatingPointCase(
      radix = matrix.radix,
      check = matrix.check,
      name  = "SRAM delays",
      fft = FFTDutConfiguration(
        size           = 64,
        decimation     = decimation,
        minSRAMdepth   = matrix.minSRAMdepth,
        singlePortSRAM = singlePortSRAM,
      ),
      seedOffset = if (singlePortSRAM) 60 else 61,
    )

    baseline ++ pipeline ++ selectedSignal ++ multipleFrames ++ fourMul ++ sram
  }

  private val radixCaseConfigs: Iterator[FloatingPointCase] =
    radixMatrices.iterator.flatMap(radixConfigs)

  private val stageGrowthConfigs: Iterator[FloatingPointCase] = {
    val configs = for {
      radix         <- Seq(Radix2, Radix22).iterator
      size          <- stageGrowthSizeSeq.iterator
      decimation    <- decimationSeq.iterator
      growthPattern <- stageGrowthPatternSeq.iterator
    } yield {
      val growEnable = growthPattern.grow(log2Up(size))
      FloatingPointCase(
        radix      = radix,
        check      = FirstValidOutputFrame,
        name       = "stage growth",
        fft        = FFTDutConfiguration(size = size, decimation = decimation, growEnable = growEnable),
        seedOffset = 120 + growthPattern.seedOffset + (if (radix == Radix22) 20 else 0) + (if (size == 1024) 10 else 0),
        growName   = growthPattern.name,
      )
    }
    configs
  }

  (radixCaseConfigs ++ stageGrowthConfigs).foreach { config =>
    it should TestUtils.passWhen(titleFields(config): _*) in {
      val params = paramsFor(config)
      assertGrowthOutputFormat(config, params)
      val pattern = patternFor(config)
      val plotName = plotNameFor(config, pattern)

      test(new FFT(params))
        .withAnnotations(annotationsFor(config.fft.size))
        .runPeekPoke(dut =>
          new FFTvsFloatingPointTester(
            dut         = dut,
            params      = params,
            check       = config.check,
            pattern     = pattern,
            inputFrames = config.inputFrames,
            tol         = 0.02,
            plotName    = plotName,
          )
        )
    }
  }

  private def paramsFor(config: FloatingPointCase): FFTParams =
    FFTModelTestUtils.fftParams(
      radix          = config.radix,
      size           = config.fft.size,
      decimation     = config.fft.decimation,
      use4Muls       = config.fft.use4Muls,
      numAddPipes    = config.fft.addPipeRegs,
      numMulPipes    = config.fft.mulPipeRegs,
      minSRAMdepth   = config.fft.minSRAMdepth,
      singlePortSRAM = config.fft.singlePortSRAM,
      growEnable     = config.fft.growEnable,
    )

  private def patternFor(config: FloatingPointCase): InputPatterns.FftFramePattern = {
    val patterns = InputPatterns.standardFftPatterns(
      config.fft.size,
      dutAmplitudeRaw,
      noiseSeed         = seed(config),
      noiseAmplitudeRaw = dutNoiseAmplitudeRaw
    )
    val noisyPatterns = patterns.filter(_.noise.nonEmpty)
    if (config.name == "selected non-sparse signal") {
      noisyPatterns.find(_.label == "multi-tone-noise").getOrElse(noisyPatterns.head)
    } else {
      noisyPatterns(math.floorMod(config.seedOffset, noisyPatterns.length))
    }
  }

  private def titleFields(config: FloatingPointCase): Seq[(String, Any)] =
    TestUtils.titleFields(
      Seq(
        "check"      -> checkName(config),
        "sdfRadix"   -> config.radix.label,
        "size"       -> config.fft.size,
        "decimation" -> config.fft.decimation,
      ),
      (config.fft.addPipeRegs != 1)    -> ("addPipeRegs" -> config.fft.addPipeRegs),
      (config.fft.mulPipeRegs != 1)    -> ("mulPipeRegs" -> config.fft.mulPipeRegs),
      config.fft.use4Muls              -> ("use4Muls" -> config.fft.use4Muls),
      (config.fft.minSRAMdepth > 0)    -> ("minSRAMdepth" -> config.fft.minSRAMdepth),
      (config.fft.minSRAMdepth > 0)    -> ("singlePortSRAM" -> config.fft.singlePortSRAM),
      (config.inputFrames != 2)        -> ("inputFrames" -> config.inputFrames),
      config.fft.growEnable.nonEmpty   -> ("grow" -> growDescription(config)),
    )

  private def checkName(config: FloatingPointCase): String =
    if (config.name.nonEmpty) config.name else config.check.label

  private def plotNameFor(config: FloatingPointCase, pattern: InputPatterns.FftFramePattern): String =
    s"fft-vs-floating-point-${config.radix.label}-${config.fft.decimation}-${config.fft.size}-" +
      s"${checkName(config)}-${pattern.label}-add${config.fft.addPipeRegs}-mul${config.fft.mulPipeRegs}-" +
      s"frames${config.inputFrames}-seed${config.seedOffset}-4mul${config.fft.use4Muls}-" +
      s"sram${config.fft.minSRAMdepth}-${config.fft.singlePortSRAM}-${growPlotSuffix(config)}"

  private def assertGrowthOutputFormat(config: FloatingPointCase, params: FFTParams): Unit =
    if (config.fft.growEnable.nonEmpty) {
      val inputWidth = params.inDataType.real.getWidth
      val expectedWidth = inputWidth + config.fft.growEnable.count(identity)
      val inputBinaryPoint = params.inDataType.real.binaryPoint

      assert(
        params.fftOutputType.real.getWidth == expectedWidth,
        s"growth output real width mismatch: expected $expectedWidth, got ${params.fftOutputType.real.getWidth}"
      )
      assert(
        params.fftOutputType.imag.getWidth == expectedWidth,
        s"growth output imag width mismatch: expected $expectedWidth, got ${params.fftOutputType.imag.getWidth}"
      )
      assert(
        params.fftOutputType.real.binaryPoint == inputBinaryPoint,
        s"growth output real binary point mismatch: expected $inputBinaryPoint, got ${params.fftOutputType.real.binaryPoint}"
      )
      assert(
        params.fftOutputType.imag.binaryPoint == inputBinaryPoint,
        s"growth output imag binary point mismatch: expected $inputBinaryPoint, got ${params.fftOutputType.imag.binaryPoint}"
      )
    }

  private def growDescription(config: FloatingPointCase): String =
    s"${config.growName} ${growBits(config)}"

  private def growPlotSuffix(config: FloatingPointCase): String =
    if (config.fft.growEnable.isEmpty) "grow-default" else s"grow-${config.growName}-${growBits(config)}"

  private def growBits(config: FloatingPointCase): String =
    config.fft.growEnable.map(enabled => if (enabled) "1" else "0").mkString

  private def seed(config: FloatingPointCase): Long =
    0x5eed1234L + 131L * config.fft.size + 17L * config.seedOffset + (if (config.fft.decimation == DIT) 1L else 0L)

  private def annotationsFor(size: Int) =
    if (size >= 512) TestConfig.nonParallelVerilatorAnnotations else TestConfig.annotations
}
