package windowing

import chisel3.Data
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}
import fixedpoint.FixedPoint

import java.io.{BufferedWriter, File, FileWriter}
import scala.math.BigDecimal.double2bigDecimal

object Utils {
  def roundTiesToEven(x: Double): Int = {
    if (x < 0) {
      // Use symmetry for negative numbers
      -roundTiesToEven(-x)
    } else {
      val intPart = math.floor(x)
      val fracPart = x - intPart
      (intPart.toInt +
        (if (fracPart > 0.5 ||
          (fracPart == 0.5 && intPart % 2 == 1)) 1 else 0))
    }
  }
  def roundWithMode(x: Double, mode: TrimType): Double = {
    if (x < 0) -roundWithMode(-x, mode)
    else mode match {
      case Ceiling => math.ceil(x)
      case Floor => math.floor(x)
      case Convergent =>
        // Bankers' rounding: round to nearest even integer on .5
        val floor = math.floor(x)
        val frac = x - floor
        floor + (if (frac > 0.5 || (frac == 0.5 && floor % 2 == 1)) 1 else 0)
      case Round =>
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

  def toSignedNBits(x: BigInt, n: Int): BigInt = {
    val mask = (BigInt(1) << n) - 1
    val masked = x & mask
    // If sign bit is set, subtract 2^n to get negative value
    if ((masked & (BigInt(1) << (n - 1))) != 0)
      masked - (BigInt(1) << n)
    else
      masked
  }

  def formatString(data: BigInt, dataBytes: Int): String = {
    // Determine how many hex numbers wy need to print dataBytes number of Bytes
    val hexNumbers = dataBytes * 2
    // Convert BigInt to uppercase Hex
    val peekedString = data.toString(16).toUpperCase
    // Fill with zeroes
    if (peekedString.length >= hexNumbers) peekedString
    else "0" * (hexNumbers - peekedString.length) + peekedString
  }
  def writeWindowFunction2File(fileName: String, dataType: Data, window: Seq[Double], dataPerWord: Int = 1, dataBytes: Int): Unit = {
    val binPointPosition = dataType match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    }

    val file = new File(fileName)

    // Create parent directories if they don't exist
    file.getParentFile.mkdirs()

    val w = new BufferedWriter(new FileWriter(file))
    val windowShifted = window.map(
      c => formatString(roundWithMode(c * (1 << binPointPosition), Convergent).toBigInt, dataBytes)
    )

    windowShifted.grouped(dataPerWord).foreach { m => w.write(m.mkString + "\n") }
    w.close()
  }
}
