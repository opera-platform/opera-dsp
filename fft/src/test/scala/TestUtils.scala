package opera.fft

object TestUtils {
  def passWhen(fields: (String, Any)*): String =
    "pass when:\n" + fields.map { case (key, value) => s"\t\t$key = $value" }.mkString(",\n") + "\n"

  // Reference ordering helpers used by BitReverseTester to build the expected
  // sample stream after the DUT emits a bit-reversed frame.
  def bitReverse[T](samples: Seq[T]): Vector[T] = {
    val width = chisel3.util.log2Up(samples.length)
    samples.indices.map(i => samples(bitReverseIndex(i, width))).toVector
  }

  def bitReverseIndex(index: Int, width: Int): Int = {
    var in = index
    var out = 0
    for (_ <- 0 until width) {
      out = (out << 1) | (in & 1)
      in = in >> 1
    }
    out
  }
}

object SDFStageInputPatterns {
  import FFTStageModel.{FixedFormat, RawComplex}

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
}
