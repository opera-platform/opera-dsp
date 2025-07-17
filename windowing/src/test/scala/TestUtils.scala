package opera.windowing

import dsptools.TrimType
import dsptools.numbers.Convergent
import opera.common.ArithmeticUtils

import scala.math.BigDecimal.double2bigDecimal

trait TestUtils {
  def windowModel (data: BigInt, coefficient: Double, dataWidth: Int, winBinPoint: Int, trimType: TrimType): (BigInt, BigInt) = {
    val real = ArithmeticUtils.toSignedNBits(data >> (dataWidth / 2), dataWidth / 2)
    val imag = ArithmeticUtils.toSignedNBits(data & ((1 << (dataWidth / 2)) - 1), dataWidth / 2)
    val coef = ArithmeticUtils.roundWithMode(coefficient * (1 << winBinPoint), Convergent)
    val tmpReal = real.toDouble * coef / scala.math.pow(2, winBinPoint)
    val tmpImag = imag.toDouble * coef / scala.math.pow(2, winBinPoint)
    val outReal = ArithmeticUtils.roundWithMode(tmpReal, trimType).toBigInt
    val outImag = ArithmeticUtils.roundWithMode(tmpImag, trimType).toBigInt
    (outReal, outImag)
  }
}
