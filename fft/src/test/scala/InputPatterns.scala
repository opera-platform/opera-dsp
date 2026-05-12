package opera.fft

import ModelUtils.{FixedFormat, RawComplex}

object InputPatterns {
  /**
   * One complex tone in a generated FFT frame.
   *
   * @param bin          FFT bin index. Must be nonzero and smaller than the FFT size.
   * @param amplitudeRaw Tone amplitude in raw FixedPoint units.
   * @param phaseRadians Initial tone phase in radians.
   */
  final case class FftTone(bin: Int, amplitudeRaw: BigInt, phaseRadians: Double = 0.0)

  /**
   * Optional deterministic raw noise added independently to real and imaginary lanes.
   *
   * @param maxAmplitudeRaw Maximum absolute raw noise value per lane.
   * @param seed            Random seed for repeatable noise.
   */
  final case class FftNoise(maxAmplitudeRaw: Int, seed: Long)

  /**
   * One natural-order FFT input frame pattern.
   *
   * @param label Readable pattern label used in test names and plot names.
   * @param tones Complex tones to sum into the frame.
   * @param dc    Constant raw complex offset added to every sample.
   * @param noise Optional deterministic raw uniform noise.
   */
  final case class FftFramePattern(
      label: String,
      tones: Seq[FftTone],
      dc   : RawComplex = RawComplex(0, 0),
      noise: Option[FftNoise] = None,
  )

  /**
   * Creates deterministic low-amplitude raw input frames for DUT/model tests.
   *
   * @param params       FFT parameters that define the input format.
   * @param seed         Random seed for repeatable tests.
   * @param frames       Number of FFT frames to generate.
   * @param amplitudeRaw Maximum absolute raw value for each component.
   */
  def deterministicFftFrames(params: FFTParams, seed: Long, frames: Int, amplitudeRaw: Int = 96): Vector[RawComplex] = {
    val format = ModelUtils.formatOf(params.inDataType)
    val rng    = new scala.util.Random(seed)
    Vector.fill(frames * params.fftSize) {
      val real = BigInt(rng.nextInt(2 * amplitudeRaw + 1) - amplitudeRaw)
      val imag = BigInt(rng.nextInt(2 * amplitudeRaw + 1) - amplitudeRaw)
      RawComplex(format.wrap(real), format.wrap(imag))
    }
  }

  /**
   * Creates one natural-order FFT frame from a typed tone/noise pattern.
   *
   * @param params  FFT parameters that define size and input format.
   * @param pattern Tone, DC, and optional noise pattern to synthesize.
   */
  def fftFrame(params: FFTParams, pattern: FftFramePattern): Vector[RawComplex] =
    fftFrame(ModelUtils.formatOf(params.inDataType), params.fftSize, pattern)

  /**
   * Creates one natural-order FFT frame from a typed tone/noise pattern.
   *
   * @param format  FixedPoint input format used to wrap raw samples.
   * @param size    Number of samples in the FFT frame.
   * @param pattern Tone, DC, and optional noise pattern to synthesize.
   */
  def fftFrame(format: FixedFormat, size: Int, pattern: FftFramePattern): Vector[RawComplex] = {
    require(size > 1, s"FFT frame size must be greater than 1, got $size")
    require(pattern.label.nonEmpty, "FFT frame pattern label must be non-empty")
    pattern.tones.foreach { tone =>
      require(tone.bin >= 1 && tone.bin < size, s"tone bin ${tone.bin} must satisfy 1 <= bin < $size")
      require(tone.amplitudeRaw >= 0, s"tone amplitude must be non-negative, got ${tone.amplitudeRaw}")
    }
    pattern.noise.foreach { noise =>
      require(noise.maxAmplitudeRaw >= 0, s"noise amplitude must be non-negative, got ${noise.maxAmplitudeRaw}")
    }

    val rng = pattern.noise.map(noise => new scala.util.Random(noise.seed))
    val noiseAmplitude = pattern.noise.map(_.maxAmplitudeRaw).getOrElse(0)

    Vector.tabulate(size) { index =>
      val angleScale = 2.0 * math.Pi * index.toDouble / size.toDouble
      val toneReal   = pattern.tones.map { tone =>
        tone.amplitudeRaw.toDouble * math.cos(angleScale * tone.bin.toDouble + tone.phaseRadians)
      }.sum
      val toneImag = pattern.tones.map { tone =>
        tone.amplitudeRaw.toDouble * math.sin(angleScale * tone.bin.toDouble + tone.phaseRadians)
      }.sum
      val noiseReal = rng.map(randomNoise(_, noiseAmplitude)).getOrElse(BigInt(0))
      val noiseImag = rng.map(randomNoise(_, noiseAmplitude)).getOrElse(BigInt(0))

      RawComplex(
        format.wrap(pattern.dc.real + roundRaw(toneReal) + noiseReal),
        format.wrap(pattern.dc.imag + roundRaw(toneImag) + noiseImag)
      )
    }
  }

  /**
   * Selects the default single-tone bin for a given FFT size.
   *
   * The bin is derived from the FFT size, kept nonzero, and kept below Nyquist
   * when the FFT size has a distinct below-Nyquist bin.
   */
  def singleToneBin(size: Int): Int = {
    require(size > 1, s"FFT size must be greater than 1, got $size")
    val belowNyquistMax = math.max(1, size / 2 - 1)
    clampBin(math.max(1, size / 8), belowNyquistMax)
  }

  /**
   * Selects default multi-tone bins for a given FFT size.
   *
   * Bins start from size-derived fractions and then fill from the lowest
   * available nonzero bins if small FFTs collapse those fractions to duplicates.
   */
  def multiToneBins(size: Int, count: Int = 3): Vector[Int] = {
    require(size > 1, s"FFT size must be greater than 1, got $size")
    require(count > 0, s"multi-tone bin count must be positive, got $count")

    val initial = Seq(size / 16, size / 8, (3 * size) / 16)
      .map(bin => clampBin(math.max(1, bin), size - 1))
      .distinct
    val fill = (1 until size).filterNot(initial.contains)
    (initial ++ fill).take(count).toVector
  }

  /**
   * Builds a typed single-tone FFT pattern.
   */
  def singleTonePattern(
      size            : Int,
      baseAmplitudeRaw: BigInt,
      dc              : RawComplex = RawComplex(0, 0),
      noise           : Option[FftNoise] = None,
      label           : String = "single-tone",
  ): FftFramePattern =
    FftFramePattern(
      label = label,
      tones = Seq(FftTone(singleToneBin(size), baseAmplitudeRaw)),
      dc    = dc,
      noise = noise
    )

  /**
   * Builds a typed multi-tone FFT pattern.
   */
  def multiTonePattern(
      size            : Int,
      baseAmplitudeRaw: BigInt,
      dc              : RawComplex = RawComplex(0, 0),
      noise           : Option[FftNoise] = None,
      label           : String = "multi-tone",
  ): FftFramePattern = {
    val bins  = multiToneBins(size)
    val tones = bins.zipWithIndex.map { case (bin, index) =>
      FftTone(bin, (baseAmplitudeRaw >> index).max(BigInt(1)), phaseRadians = index.toDouble * 0.35)
    }
    FftFramePattern(label = label, tones = tones, dc = dc, noise = noise)
  }

  /**
   * Creates one single-tone FFT frame.
   */
  def singleToneFrame(
      params          : FFTParams,
      baseAmplitudeRaw: BigInt,
      dc              : RawComplex = RawComplex(0, 0),
      noise           : Option[FftNoise] = None,
  ): Vector[RawComplex] =
    fftFrame(params, singleTonePattern(params.fftSize, baseAmplitudeRaw, dc, noise))

  /**
   * Creates one multi-tone FFT frame.
   */
  def multiToneFrame(
      params          : FFTParams,
      baseAmplitudeRaw: BigInt,
      dc              : RawComplex = RawComplex(0, 0),
      noise           : Option[FftNoise] = None,
  ): Vector[RawComplex] =
    fftFrame(params, multiTonePattern(params.fftSize, baseAmplitudeRaw, dc, noise))

  /**
   * Standard tone/noise FFT pattern family used by model and DUT comparison tests.
   */
  def standardFftPatterns(size: Int, baseAmplitudeRaw: BigInt, noiseSeed: Long): Vector[FftFramePattern] =
    standardFftPatterns(size, baseAmplitudeRaw, noiseSeed, defaultNoiseAmplitude(baseAmplitudeRaw))

  /**
   * Standard tone/noise FFT pattern family used by model and DUT comparison tests.
   *
   * @param size              FFT frame size.
   * @param baseAmplitudeRaw  Base raw tone amplitude.
   * @param noiseSeed         Random seed used by deterministic noisy variants.
   * @param noiseAmplitudeRaw Maximum absolute raw noise value per real/imag lane.
   */
  def standardFftPatterns(
      size             : Int,
      baseAmplitudeRaw : BigInt,
      noiseSeed        : Long,
      noiseAmplitudeRaw: Int,
  ): Vector[FftFramePattern] = {
    require(baseAmplitudeRaw > 0, s"base amplitude must be positive, got $baseAmplitudeRaw")
    require(noiseAmplitudeRaw >= 0, s"noise amplitude must be non-negative, got $noiseAmplitudeRaw")

    val dc     = RawComplex(baseAmplitudeRaw / 4, -baseAmplitudeRaw / 8)
    val single = singleTonePattern(size, baseAmplitudeRaw)
    val multi  = multiTonePattern(size, baseAmplitudeRaw)

    Vector(
      single,
      multi,
      single.copy(label = "single-tone-noise", noise = Some(FftNoise(noiseAmplitudeRaw, noiseSeed))),
      multi.copy (label = "multi-tone-noise", noise = Some(FftNoise(noiseAmplitudeRaw, noiseSeed + 1))),
      single.copy(label = "dc-single-tone", dc = dc),
      single.copy(label = "dc-single-tone-noise", dc = dc, noise = Some(FftNoise(noiseAmplitudeRaw, noiseSeed + 2))),
    )
  }

  /**
   * Creates paired SDF samples that exercise signs and near-corner raw values.
   *
   * @param format    FixedPoint format used to encode the samples.
   * @param stageSize Number of samples in one stage period.
   */
  def safeCornerPattern(format: FixedFormat, stageSize: Int): Vector[RawComplex] =
    pairedStagePattern(
      stageSize,
      Seq(
        RawComplex(0, 0)                                 -> RawComplex(0, 0),
        RawComplex(1, -1)                                -> RawComplex(0, 0),
        RawComplex(-1, 1)                                -> RawComplex(0, 0),
        RawComplex(2, -2)                                -> RawComplex(1, -1),
        RawComplex(3, -3)                                -> RawComplex(0, 0),
        RawComplex(format.maxRaw / 2, format.minRaw / 2) -> RawComplex(1, -1),
        RawComplex(format.minRaw / 2, format.maxRaw / 2) -> RawComplex(-1, 1),
        RawComplex(format.maxRaw / 2 - 1, 1)             -> RawComplex(1, format.maxRaw / 2 - 1),
        RawComplex(format.minRaw / 2 + 1, -1)            -> RawComplex(-1, format.minRaw / 2 + 1),
      )
    )

  /**
   * Creates paired SDF samples intended to trigger overflow checks.
   *
   * @param format    FixedPoint format used to encode the samples.
   * @param stageSize Number of samples in one stage period.
   */
  def overflowPattern(format: FixedFormat, stageSize: Int): Vector[RawComplex] =
    pairedStageFramesPattern(
      stageSize,
      Seq(
        RawComplex(format.maxRaw, 0) -> RawComplex(1, 0),
        RawComplex(format.minRaw, 0) -> RawComplex(-1, 0),
        RawComplex(format.maxRaw, 0) -> RawComplex(-1, 0),
        RawComplex(format.minRaw, 0) -> RawComplex(1, 0),
        RawComplex(0, format.maxRaw) -> RawComplex(0, 1),
        RawComplex(0, format.minRaw) -> RawComplex(0, -1),
        RawComplex(0, format.maxRaw) -> RawComplex(0, -1),
        RawComplex(0, format.minRaw) -> RawComplex(0, 1),
      )
    )

  /**
   * Creates deterministic random SDF input frames.
   *
   * @param format    FixedPoint format used to encode the samples.
   * @param stageSize Number of samples in one stage period.
   * @param seed      Random seed for repeatable tests.
   * @param frames    Number of stage periods to generate.
   */
  def seededPattern(format: FixedFormat, stageSize: Int, seed: Long, frames: Int): Vector[RawComplex] = {
    require(frames > 0, "seededPattern requires at least one frame")

    val rng = new scala.util.Random(seed)
    Vector.fill(frames)(()).flatMap { _ =>
      val pairs = Vector.fill(stageSize / 2)(randomSample(format, rng) -> randomSample(format, rng))
      pairedStagePattern(stageSize, pairs)
    }
  }

  /**
   * Creates a combined corner, optional overflow, and random stress pattern.
   *
   * @param format          FixedPoint format used to encode the samples.
   * @param stageSize       Number of samples in one stage period.
   * @param seed            Random seed for repeatable tests.
   * @param frames          Number of random stage periods to append.
   * @param includeOverflow If `true`, includes samples that should exercise overflow behavior.
   */
  def strictPattern(format: FixedFormat, stageSize: Int, seed: Long, frames: Int, includeOverflow: Boolean): Vector[RawComplex] = {
    val corners = safeCornerPattern(format, stageSize)
    val overflow = if (includeOverflow) overflowPattern(format, stageSize) else Vector.empty
    corners ++ overflow ++ seededPattern(format, stageSize, seed, frames)
  }

  private def pairedStagePattern(stageSize: Int, pairs: Seq[(RawComplex, RawComplex)]): Vector[RawComplex] = {
    require(stageSize >= 2 && stageSize % 2 == 0, s"stageSize must be an even value >= 2, got $stageSize")
    require(pairs.nonEmpty, "pairedStagePattern requires at least one pair")

    val delay = stageSize / 2
    val selectedPairs = Iterator.continually(pairs).flatten.take(delay).toVector
    selectedPairs.map(_._1) ++ selectedPairs.map(_._2)
  }

  private def pairedStageFramesPattern(stageSize: Int, pairs: Seq[(RawComplex, RawComplex)]): Vector[RawComplex] = {
    require(stageSize >= 2 && stageSize % 2 == 0, s"stageSize must be an even value >= 2, got $stageSize")
    require(pairs.nonEmpty, "pairedStageFramesPattern requires at least one pair")

    val delay = stageSize / 2
    pairs.grouped(delay).toVector.flatMap(group => pairedStagePattern(stageSize, group))
  }

  private def randomSample(format: FixedFormat, rng: scala.util.Random): RawComplex =
    RawComplex(randomRaw(format, rng), randomRaw(format, rng))

  private def randomRaw(format: FixedFormat, rng: scala.util.Random): BigInt =
    format.wrap(BigInt(format.width, rng))

  private def randomNoise(rng: scala.util.Random, amplitude: Int): BigInt =
    if (amplitude == 0) BigInt(0) else BigInt(rng.nextInt(2 * amplitude + 1) - amplitude)

  private def defaultNoiseAmplitude(baseAmplitudeRaw: BigInt): Int =
    math.max(1, (baseAmplitudeRaw / 32).toInt)

  private def roundRaw(value: Double): BigInt =
    BigDecimal.decimal(value).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt

  private def clampBin(bin: Int, max: Int): Int =
    math.max(1, math.min(bin, max))
}
