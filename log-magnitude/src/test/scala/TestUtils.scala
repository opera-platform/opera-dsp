package opera.logmagnitude

import breeze.linalg._
import breeze.numerics.{abs, log}
import dsptools.TrimType
import opera.common.ArithmeticUtils

import scala.math.BigDecimal.double2bigDecimal

trait TestUtils {
 def jplModel (real: Long, imag: Long, inputBinPoint: Int, outputBinPoint: Int, trimType: TrimType): BigInt = {
   // Get I (real) and Q (imaginary) absolute values
   val absI = abs(real)
   val absQ = abs(imag)
   // Calculate X and Y
   val x = max(absI, absQ)
   val y = min(absI, absQ)
   // A = 1.0 * X + 1/8 * Y;  X >= 3Y
   val geA = x + (y >> 3)
   // We want to avoid multiplication 7/8 * X. So we will instead subtract 1/8*X from X
   val x_7_8 = x - (x >> 3)
   // A = 7/8 * X + 1/2 * Y;  X <= 3Y
   val leA = x_7_8 + (y >> 1)
   // Scale and round if necessary
   val A = if (x >= 3*y) geA else leA
   if (inputBinPoint > outputBinPoint) {
     val scaled = A.toDouble / scala.math.pow(2, inputBinPoint - outputBinPoint)
     ArithmeticUtils.roundWithMode(scaled, trimType).toBigInt
   } else {
     BigInt(A)
   }
 }

  def squareModel(real: Long, imag: Long, inputBinPoint: Int, outputBinPoint: Int, trimType: TrimType): BigInt = {
    // Get I (real) and Q (imaginary) absolute values
    val absI = abs(real)
    val absQ = abs(imag)
    // Calculate I*I and Q*Q
    val squareI = absI*absI
    val squareQ = absQ*absQ
    val squared = BigInt(squareI + squareQ)
    // Scale and round
    val scaled =
      if (2 * inputBinPoint > outputBinPoint)
        squared.toDouble / scala.math.pow(2, 2 * inputBinPoint - outputBinPoint)
      else
        squared.toDouble
    val out = ArithmeticUtils.roundWithMode(scaled, trimType).toBigInt
    out
  }

  def logModel(data: BigInt, inputBinPoint: Int, lutTableWidth: Int, outputBinPoint: Int, lutTableSize: Int, trimType: TrimType): Double = {
    val log2 =
      if (data == 0) {
        -inputBinPoint
      } else {
        val leadingOne = data.bitLength - 1
        val cropBits = leadingOne - lutTableSize
        val mCropped = if (cropBits > 0) {
          val mask = ~((BigInt(1) << cropBits) - 1)
          data & mask
        } else { data }

        log(mCropped.toDouble / scala.math.pow(2, inputBinPoint)) / log(2)
      }

    val log2scaled = log2 * scala.math.pow(2, lutTableWidth)
    val log2rounded = ArithmeticUtils.roundWithMode(log2scaled, trimType).toBigInt
    if (lutTableWidth < outputBinPoint)
      log2rounded.toDouble / scala.math.pow(2, lutTableWidth)
    else {
      val scaled = log2rounded.toDouble / scala.math.pow(2, lutTableWidth - outputBinPoint)
      val rounded = ArithmeticUtils.roundWithMode(scaled, trimType).toBigInt
      rounded.toDouble * scala.math.pow(2, lutTableWidth - outputBinPoint) / scala.math.pow(2, lutTableWidth)
    }
  }
}