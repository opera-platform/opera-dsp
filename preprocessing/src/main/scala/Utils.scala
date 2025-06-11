package opera.preprocessing

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
      val maxChirpSize         = (content \ "parameters" \ "MaxChirpSize").get.as[Int]
      val maxChirpsPerFrame = (content \ "parameters" \ "MaxChirpsPerFrame").get.as[Int]
      // Read CRC parameters
      val crcParams = CRCParameters(
        dataBytes  = (content \ "crcParameters" \ "dataBytes").get.as[Int],
        polynomial = java.lang.Long.parseUnsignedLong(
          (content \ "crcParameters" \ "polynomial").get.as[String].stripSuffix("L").drop(2), 16
        ),
        init = java.lang.Long.parseUnsignedLong(
          (content \ "crcParameters" \ "init").get.as[String].stripSuffix("L").drop(2), 16
        ),
        reflectIn  = (content \ "crcParameters" \ "reflectIn").get.as[Boolean],
        reflectOut = (content \ "crcParameters" \ "reflectOut").get.as[Boolean],
        xorOut = java.lang.Long.parseUnsignedLong(
          (content \ "crcParameters" \ "xorOut").get.as[String].stripSuffix("L").drop(2), 16
        ),
      )
      // Read Buffer parameters
      val bufferParams = BufferParameters(
        insertBuffers = (content \ "bufferParameters" \ "insertBuffers").get.as[Boolean],
        size          = (content \ "bufferParameters" \ "size").get.as[Int],
      )

      Left((
        address,
        PreProcessingParameters(
          MaxChirpSize = maxChirpSize,
          MaxChirpsPerFrame = maxChirpsPerFrame,
          CrcParams = crcParams,
          BufferParams = bufferParams
        )
      ))
    } catch {
      case e: FileNotFoundException => throw e
      case e: Throwable =>
        Right(throw new Exception(f"Exception occured when parsing configuration file: ${e.getMessage}"))
    }
  }
}
