package opera.logmagnitude

import chisel3.fromIntToWidth
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}
import freechips.rocketchip.diplomacy.AddressSet
import play.api.libs.json.Json

import java.io.FileNotFoundException

object ParseParameters {
  def parseconfig(filename: String) = {
    try {
      val resource = scala.io.Source.fromFile(filename)
      val content = Json.parse(resource.getLines().mkString)
      // Read Address
      val address = AddressSet(
        base = BigInt((content \ "address" \ "base").get.as[String].stripSuffix("L").drop(2), 16),
        mask = BigInt((content \ "address" \ "mask").get.as[String].stripSuffix("L").drop(2), 16)
      )
      // Read Parameters
      val magnitudeParams = {
        LogMagnitudeParams(
          inputType = DspComplex(
            FixedPoint(
              (content \ "parameters" \ "inputWidth").get.as[Int].W,
              (content \ "parameters" \ "inputBinPoint").get.as[Int].BP
            )
          ),
          realType = Some(FixedPoint(
            (content \ "parameters" \ "realWidth").get.as[Int].W,
            (content \ "parameters" \ "realBinPoint").get.as[Int].BP
          )),
          outputType = FixedPoint(
            (content \ "parameters" \ "outputWidth").get.as[Int].W,
            (content \ "parameters" \ "outputBinPoint").get.as[Int].BP
          ),
          lutTableSize = Some((content \ "parameters" \ "lutTableSize").get.as[Int]),
          lutTableWidth = Some((content \ "parameters" \ "lutTableWidth").get.as[Int]),
          magType = {
            (content \ "parameters" \ "magType").get.as[String] match {
              case "JPL"           => JPL
              case "Squared"       => Squared
              case "Log"           => Log
              case "LogSquaredJPL" => LogSquaredJPL
              case "LogJPLSquared" => LogJPLSquared
              case _               => JPL
            }
          },
          addPipeRegs = (content \ "parameters" \ "addPipeRegs"  ).get.as[Boolean],
          mulPipeRegs = (content \ "parameters" \ "mulPipeRegs"  ).get.as[Boolean],
          trimType = {
            (content \ "parameters" \ "trimType").get.as[String] match {
              case "Floor"      => Floor
              case "Ceiling"    => Ceiling
              case "Convergent" => Convergent
              case "Round"      => Round
              case _            => Convergent
            }
          },
        )
      }
      val beatBytes = (content \ "parameters" \ "beatBytes").get.as[Int]

      Left((
        address,
        magnitudeParams,
        beatBytes
      ))
    } catch {
      case e: FileNotFoundException => throw e
      case e: Throwable =>
        Right(throw new Exception(f"Exception occured when parsing configuration file: ${e.getMessage}"))
    }
  }
}
