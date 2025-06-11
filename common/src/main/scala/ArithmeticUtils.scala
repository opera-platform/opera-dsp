package opera.common

import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}

object ArithmeticUtils {
  def roundWithMode(x: Double, mode: TrimType): Double = {
    mode match {
      case Ceiling => math.ceil(x)
      case Floor => math.floor(x)
      case Convergent =>
        if (x < 0) -roundWithMode(-x, mode) else {
          // Bankers' rounding: round to nearest even integer on .5
          val floor = math.floor(x)
          val frac = x - floor
          floor + (if (frac > 0.5 || (frac == 0.5 && floor % 2 == 1)) 1 else 0)
        }
      case Round =>
        if (x < 0) -roundWithMode(-x, mode) else {
          // Round half away from zero (up for positive, down for negative)
          val floor = math.floor(x)
          val frac = x - floor
          if (math.abs(frac) == 0.5) {
            if (x > 0) floor + 1
            else floor - 1
          } else {
            math.round(x).toDouble
          }
        }
    }
  }

  def toSignedNBits(x: BigInt, n: Int): BigInt = {
    val mask = (BigInt(1) << n) - 1
    val masked = x & mask
    // If sign bit is set, subtract 2^n to get negative value
    if ((masked & (BigInt(1) << (n - 1))) != 0)
      masked - (BigInt(1) << n)
    else
      masked
  }
}
