package opera.lis

import chisel3._
import dsptools.numbers._
import fixedpoint._

private[lis] object LISNumeric {
  def resetValue[T <: Data: Real](proto: T, useHigh: Boolean): T = {
    val w_reset = Wire(proto.cloneType)
    w_reset := Real[T].fromDouble(boundaryValue(proto, useHigh))
    w_reset
  }

  private def boundaryValue[T <: Data](proto: T, useHigh: Boolean): Double = proto match {
    case f: FixedPoint =>
      val integerBits = f.getWidth - f.binaryPoint.get - 1
      if (useHigh) math.pow(2.0, integerBits) - math.pow(2.0, -f.binaryPoint.get)
      else -math.pow(2.0, integerBits)
    case s: SInt =>
      if (useHigh) math.pow(2.0, s.getWidth - 1) - 1.0
      else -math.pow(2.0, s.getWidth - 1)
    case u: UInt =>
      if (useHigh) math.pow(2.0, u.getWidth) - 1.0
      else 0.0
    case other =>
      throw new IllegalArgumentException(s"Unsupported LIS numeric type: ${other.getClass.getName}")
  }
}
