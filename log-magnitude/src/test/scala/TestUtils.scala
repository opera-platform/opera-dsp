package opera.logmagnitude

import breeze.linalg._
import breeze.numerics.abs

trait TestUtils {
 def jpl (real: Long, imag: Long): BigInt = {
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
   // A= 7/8 * X + 1/2 * Y;  X <= 3Y
   val leA = x_7_8 + (y >> 1)
   val A = max(geA, leA)
   BigInt(A)
 }

  def square(real: Long, imag: Long): BigInt = {
    // Get I (real) and Q (imaginary) absolute values
    val absI = abs(real)
    val absQ = abs(imag)
    // Calculate I*I and Q*Q
    val squareI = absI*absI
    val squareQ = absQ*absQ
    // Return I*I and Q*Q
    BigInt(squareI + squareQ)
  }

}