package opera.windowing

import chisel3.{Data, fromIntToWidth}
import dsptools.numbers.{Ceiling, Convergent, DspComplex, Floor, Round}
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}
import freechips.rocketchip.diplomacy.AddressSet
import opera.common.ArithmeticUtils.roundWithMode
import opera.common.StringUtils.formatString
import play.api.libs.json.Json

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import scala.math.BigDecimal.double2bigDecimal

object Utils {
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
  def parseconfig(filename: String): Either[(AddressSet, AddressSet, WindowingParams[FixedPoint]), Nothing] = {
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
        inputType = DspComplex(
          FixedPoint(
            (content \ "parameters" \ "inputWidth").get.as[Int].W,
            (content \ "parameters" \ "inputBinPoint").get.as[Int].BP
          )
        ),
        outputType = DspComplex(
          FixedPoint(
            (content \ "parameters" \ "outputWidth").get.as[Int].W,
            (content \ "parameters" \ "outputBinPoint").get.as[Int].BP
          )
        ),
        coeffType = FixedPoint(
          (content \ "parameters" \ "coeffWidth").get.as[Int].W,
          (content \ "parameters" \ "coefBinPoint").get.as[Int].BP
        ),
        numPoints = numPoints,
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
