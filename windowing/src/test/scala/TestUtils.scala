package opera.windowing

import dsptools.TrimType
import dsptools.numbers.Convergent
import opera.common.ArithmeticUtils

import scala.math.BigDecimal.double2bigDecimal

trait TestUtils {
  def windowModel (
    inputData     : BigInt,
    coefficient   : Double,
    inputWidth    : Int,
    inputBinPoint : Int,
    outputBinPoint: Int,
    coeffBinPoint : Int,
    trimType      : TrimType
  ): (BigInt, BigInt) = {
    val real = ArithmeticUtils.toSignedNBits(inputData >> inputWidth, inputWidth)
    val imag = ArithmeticUtils.toSignedNBits(inputData & ((1 << inputWidth) - 1), inputWidth)
    val coef = ArithmeticUtils.roundWithMode(coefficient * (1 << coeffBinPoint), Convergent)
    // Scale date if necessary
    val scaledReal =
      if (inputBinPoint + coeffBinPoint > outputBinPoint) {
        real.toDouble * coef / scala.math.pow(2, inputBinPoint + coeffBinPoint - outputBinPoint)
      } else {
        real.toDouble * coef
      }
    val scaledImag =
      if (inputBinPoint + coeffBinPoint > outputBinPoint) {
        imag.toDouble * coef / scala.math.pow(2, inputBinPoint + coeffBinPoint - outputBinPoint)
      } else {
        imag.toDouble * coef
      }
    // Round the inputData
    val outReal = ArithmeticUtils.roundWithMode(scaledReal, trimType).toBigInt
    val outImag = ArithmeticUtils.roundWithMode(scaledImag, trimType).toBigInt
    (outReal, outImag)
  }
}
