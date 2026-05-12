package opera.fft

import breeze.math.Complex
import dsptools._
import dsptools.numbers.DspComplex
import fixedpoint.{FixedPoint, KnownBinaryPoint}

object ModelUtils {
  /**
   * FixedPoint format used by the pure Scala FFT models.
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

    def toDouble(raw: BigInt): Double =
      raw.toDouble / math.pow(2.0, binaryPoint.toDouble)

    def fromDouble(value: Double): BigInt = {
      val scaled = BigDecimal.decimal(value) * BigDecimal(2).pow(binaryPoint)
      require(scaled.isWhole, s"value $value is not exactly representable with binary point $binaryPoint")
      val raw = scaled.toBigIntExact.get
      require(fits(raw), s"value $value raw=$raw does not fit in $width.W, $binaryPoint.BP")
      raw
    }
  }

  object FixedFormat {
    def from(dataType: DspComplex[FixedPoint]): FixedFormat =
      formatOf(dataType)
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

  def formatOf(dataType: DspComplex[FixedPoint]): FixedFormat =
    dataType.real match {
      case fixed: FixedPoint =>
        require(fixed.widthKnown, "FixedPoint width must be known")
        fixed.binaryPoint match {
          case KnownBinaryPoint(binaryPoint) => FixedFormat(fixed.getWidth, binaryPoint)
          case other => throw new IllegalArgumentException(s"FixedPoint binary point must be known, got $other")
        }
      case other => throw new IllegalArgumentException(s"FixedPoint model supports FixedPoint only, got ${other.getClass.getName}")
    }

  def toRaw(format: FixedFormat, real: Double, imag: Double): RawComplex =
    RawComplex(format.fromDouble(real), format.fromDouble(imag))

  def rawToComplex(format: FixedFormat, sample: RawComplex): Complex =
    Complex(format.toDouble(sample.real), format.toDouble(sample.imag))

  def roundToRaw(format: FixedFormat, value: Double): BigInt = {
    val scaled = BigDecimal.decimal(value) * BigDecimal(2).pow(format.binaryPoint)
    format.wrap(scaled.setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigIntExact.get)
  }

  def roundToFittingRaw(format: FixedFormat, value: Double): BigInt = {
    val scaled = BigDecimal.decimal(value) * BigDecimal(2).pow(format.binaryPoint)
    val rounded = scaled.setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigIntExact.get
    require(format.fits(rounded), s"literal $value raw=$rounded does not fit in $format")
    rounded
  }

  def floorDiv2(raw: BigInt): BigInt =
    floorDiv(raw, BigInt(2))

  def ceilDiv2(raw: BigInt): BigInt =
    ceilDiv(raw, BigInt(2))

  def roundHalfToEvenDiv2(raw: BigInt): BigInt =
    roundShift(raw, shift = 1, RoundHalfToEven, "ModelUtils")

  def roundHalfToOddDiv2(raw: BigInt): BigInt =
    roundShift(raw, shift = 1, RoundHalfToOdd, "ModelUtils")

  def roundShift(raw: BigInt, shift: Int, trimType: TrimType, owner: String = "ModelUtils"): BigInt = {
    require(shift >= 0, s"shift must be non-negative, got $shift")
    if (shift == 0) {
      raw
    } else {
      val denom = BigInt(1) << shift
      val floor = floorDiv(raw, denom)
      val ceil = ceilDiv(raw, denom)
      val rem = raw - floor * denom
      val twiceRem = rem << 1

      trimType match {
        case NoTrim => throw new IllegalArgumentException(s"$owner does not support NoTrim when bits must be discarded")
        case RoundDown => floor
        case RoundUp => if (rem == 0) floor else ceil
        case RoundTowardsZero => raw / denom
        case RoundTowardsInfinity => if (raw >= 0) {
          if (rem == 0) floor else ceil
        } else {
          floor
        }
        case RoundHalfDown => if (twiceRem > denom) floor + 1 else floor
        case RoundHalfUp   => if (twiceRem >= denom) floor + 1 else floor
        case RoundHalfTowardsZero =>
          if (twiceRem > denom) floor + 1
          else if (twiceRem < denom) floor
          else if (raw >= 0) floor else floor + 1
        case RoundHalfTowardsInfinity =>
          if (twiceRem > denom) floor + 1
          else if (twiceRem < denom) floor
          else if (raw >= 0) floor + 1 else floor
        case RoundHalfToEven =>
          if (twiceRem > denom) floor + 1
          else if (twiceRem < denom) floor
          else if ((floor & 1) == 0) floor else floor + 1
        case RoundHalfToOdd =>
          if (twiceRem > denom) floor + 1
          else if (twiceRem < denom) floor
          else if ((floor & 1) == 1) floor else floor + 1
        case trim =>
          throw new IllegalArgumentException(s"$owner does not support trim type $trim")
      }
    }
  }

  def trimToBp(raw: BigInt, currentBp: Int, targetBp: Int, trimType: TrimType): BigInt =
    if (currentBp == targetBp) raw
    else if (currentBp > targetBp) roundShift(raw, currentBp - targetBp, trimType)
    else raw << (targetBp - currentBp)

  def complexMul(
      input      : RawComplex,
      twiddle    : RawComplex,
      inputFormat: FixedFormat,
      twFormat   : FixedFormat,
      trimType   : TrimType,
      use4Muls   : Boolean = false,
  ): RawComplex = {
    val outBp     = inputFormat.binaryPoint
    val productBp = inputFormat.binaryPoint.max(twFormat.binaryPoint) + 1

    def mul(a: BigInt, b: BigInt): BigInt =
      trimToBp(a * b, inputFormat.binaryPoint + twFormat.binaryPoint, productBp, trimType)

    val product =
      if (use4Muls) {
        val ac = mul(input.real, twiddle.real)
        val bd = mul(input.imag, twiddle.imag)
        val ad = mul(input.real, twiddle.imag)
        val bc = mul(input.imag, twiddle.real)
        RawComplex(ac - bd, ad + bc)
      } else {
        val cPlusD    = twiddle.real + twiddle.imag
        val aPlusB    = input.real + input.imag
        val bMinusA   = input.imag - input.real
        val acPlusAd  = mul(input.real, cPlusD)
        val adPlusBd  = mul(aPlusB, twiddle.imag)
        val bcMinusAc = mul(bMinusA, twiddle.real)
        RawComplex(acPlusAd - adPlusBd, acPlusAd + bcMinusAc)
      }

    RawComplex(
      inputFormat.wrap(trimToBp(product.real, productBp, outBp, trimType)),
      inputFormat.wrap(trimToBp(product.imag, productBp, outBp, trimType))
    )
  }

  def floorDiv(raw: BigInt, denom: BigInt): BigInt = if (raw >= 0) raw / denom else -(((-raw) + denom - 1) / denom)

  def ceilDiv(raw: BigInt, denom: BigInt): BigInt = -floorDiv(-raw, denom)

  private[fft] final class Pipe[A](depth: Int, init: A) {
    private val initialValues = Vector.fill(depth)(init)
    private var values = initialValues

    def out(input: A): A = if (depth == 0) input else values.last

    def shift(input: A): Unit = if (depth > 0) values = input +: values.dropRight(1)

    def step(input: A): A = {
      val output = out(input)
      shift(input)
      output
    }

    def reset(): Unit = values = initialValues
  }
}
