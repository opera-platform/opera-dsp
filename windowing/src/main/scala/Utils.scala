package windowing

import chisel3.Data
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}
import fixedpoint.FixedPoint
import freechips.rocketchip.diplomacy.AddressSet

import java.io.{BufferedWriter, File, FileWriter}
import scala.math.BigDecimal.double2bigDecimal
import play.api.libs.json.Json

import java.io.FileNotFoundException

object Utils {
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


object ParseParameters {
  def parseconfig(filename: String) = {
    try {
      val resource = scala.io.Source.fromFile(filename)
      val content  = Json.parse(resource.getLines().mkString)
      // CSR Address
      val csrAddress = AddressSet(
        base = BigInt((content \ "csrAddress" \ "base").get.as[String].stripSuffix("L").drop(2), 16),
        mask = BigInt((content \ "csrAddress" \ "mask").get.as[String].stripSuffix("L").drop(2), 16)
      )
      // RAM Address
      val ramAddress = AddressSet(
        base = BigInt((content \ "ramAddress" \ "base").get.as[String].stripSuffix("L").drop(2), 16),
        mask = BigInt((content \ "ramAddress" \ "mask").get.as[String].stripSuffix("L").drop(2), 16)
      )
      // Read Parameters
      val periodic   = (content \ "parameters" \ "periodic"  ).get.as[Boolean]
      val numPoints  = (content \ "parameters" \ "numPoints" ).get.as[Int]
      val customFile = (content \ "parameters" \ "customFile").get.as[String]
      val sigma = (content \ "parameters" \ "sigma").get.as[Double]
      val parameters = WindowingParams.fixed(
        numPoints   = numPoints,
        dataWidth   = (content \ "parameters" \ "dataWidth").get.as[Int],
        binPoint    = (content \ "parameters" \ "binPoint" ).get.as[Int],
        constWindow = (content \ "parameters" \ "constWindow").get.as[Boolean],
        trimType    = {
          (content \ "parameters" \ "trimType").get.as[String] match {
            case "Floor"      => Floor
            case "Ceiling"    => Ceiling
            case "Convergent" => Convergent
            case "Round"      => Round
            case _            => Convergent
          }
        },
        memoryFile = (content \ "parameters" \ "memoryFile").get.as[String],
        windowFunc = {
          (content \ "parameters" \ "windowFunc").get.as[String] match {
            case "TriangularWindow" => TriangularWindow(numPoints, periodic)
            case "HammingWindow"    => HammingWindow(numPoints, periodic)
            case "HanningWindow"    => HanningWindow(numPoints, periodic)
            case "BlackmanWindow"   => BlackmanWindow(numPoints, periodic)
            case "GaussianWindow"   => GaussianWindow(numPoints, sigma, periodic)
            case "NoWindow"         => NoWindow()
            case "CustomWindow"     => CustomWindow(customFile)
            case _                  => NoWindow()
          }
        }
      )
      Left((csrAddress, ramAddress, parameters))
    } catch {
      case e: FileNotFoundException => throw e
      case e: Throwable =>
        Right(throw new Exception(f"Exception occured when parsing configuration file: ${e.getMessage}"))
    }
  }
}

object AppLogger {
  private sealed abstract class Color
  private object Red extends Color
  private object Orange extends Color
  private object Green extends Color
  private object Magenta extends Color
  private val escape = "\u001b[0m"

  private val colorToAnsi = Map(
    Red -> "\u001b[31m",
    Orange -> "\u001b[38;5;208m",
    Green -> "\u001b[32m",
    Magenta -> "\u001b[35m"
  )

  private val colorToAnsiBackground = Map(
    Red -> "\u001b[41;1m",
    Orange -> "\u001b[48;5;208m",
    Green -> "\u001b[42;1m",
    Magenta -> "\u001b[45;1m"
  )

  private sealed trait LogLevel
  private case object Error extends LogLevel
  private case object Warn extends LogLevel
  private case object Info extends LogLevel
  private case object Debug extends LogLevel

  private val levelEnv = System.getenv("LOG_LEVEL")
  private val logLevel = levelEnv match {
    case "Error" => Error
    case "Warn"  => Warn
    case "Debug" => Debug
    case _       => Info
  }

  private def isDebugEnabled: Boolean = logLevel == Debug
  private def isInfoEnabled: Boolean = isDebugEnabled | logLevel == Info
  private def isWarnEnabled: Boolean = isInfoEnabled | logLevel == Warn
  private def isErrorEnabled: Boolean = isWarnEnabled | logLevel == Error

  def debug(msg: String): Unit =
    if (isDebugEnabled) println(s"[DEBUG] $msg")
  def info(msg: String): Unit =
    if (isInfoEnabled)
      println(s"${colorToAnsi(Green)}[INFO] $msg$escape")
  def warn(msg: String): Unit =
    if (isWarnEnabled)
      println(s"${colorToAnsi(Orange)}[WARN] $msg$escape")
  def error(msg: String): Unit =
    if (isErrorEnabled) println(s"${colorToAnsi(Red)}[ERROR] $msg$escape")
}