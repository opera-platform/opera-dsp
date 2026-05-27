package opera.cfar

import chisel3._
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import opera.lis.LISType
import play.api.libs.json.Json

object ParseParameters {
  def parseconfig(filename: String): Either[(AddressSet, CFARParams[FixedPoint], Int), Throwable] = {
    try {
      val resource = scala.io.Source.fromFile(filename)
      val content =
        try {
          Json.parse(resource.getLines().mkString)
        } finally {
          resource.close()
        }

      val address = AddressSet(
        base = parseHex((content \ "address" \ "base").get.as[String]),
        mask = parseHex((content \ "address" \ "mask").get.as[String])
      )

      val parameters = content \ "parameters"
      val cfarParams = CFARParams.fixed(
        inputType = FixedPoint(
          (parameters \ "inputWidth").get.as[Int].W,
          (parameters \ "inputBinPoint").get.as[Int].BP
        ),
        thresholdType = FixedPoint(
          (parameters \ "thresholdWidth").get.as[Int].W,
          (parameters \ "thresholdBinPoint").get.as[Int].BP
        ),
        scaleType = FixedPoint(
          (parameters \ "scaleWidth").get.as[Int].W,
          (parameters \ "scaleBinPoint").get.as[Int].BP
        ),
        cfarType          = parseCfarType((parameters \ "cfarType").get.as[String]),
        lisType           = parseLisType((parameters \ "lisType").get.as[String]),
        maxReferenceCells = (parameters \ "maxReferenceCells").get.as[Int],
        maxGuardCells     = (parameters \ "maxGuardCells").get.as[Int],
        maxFftSize        = (parameters \ "maxFftSize").get.as[Int],
        sendCut           = (parameters \ "sendCut").get.as[Boolean],
        logMode           = (parameters \ "logMode").get.as[Boolean],
        runtimeLogMode    = (parameters \ "runtimeLogMode").get.as[Boolean],
        edgePolicy        = parseEdgePolicy((parameters \ "edgePolicy").get.as[String]),
        runtimeEdgePolicy = (parameters \ "runtimeEdgePolicy").get.as[Boolean],
        retiming          = (parameters \ "retiming").get.as[Boolean],
        addPipeStages     = (parameters \ "addPipeStages").get.as[Int],
        mulPipeStages     = (parameters \ "mulPipeStages").get.as[Int],
        minSRAMDepth      = (parameters \ "minSRAMDepth").get.as[Int]
      )
      val beatBytes = (parameters \ "beatBytes").get.as[Int]

      Left((address, cfarParams, beatBytes))
    } catch {
      case e: Throwable => Right(e)
    }
  }

  private def parseHex(value: String): BigInt = {
    val stripped = value.stripSuffix("L").stripPrefix("0x").stripPrefix("0X")
    BigInt(stripped, 16)
  }

  private def parseCfarType(value: String): Int =
    value match {
      case "CellAveraging"    => CFARType.CellAveraging
      case "OrderedStatistic" => CFARType.OrderedStatistic
      case other              => throw new IllegalArgumentException(s"Unsupported CFAR type: $other")
    }

  private def parseLisType(value: String): String =
    value match {
      case LISType.CntBased => LISType.CntBased
      case LISType.RegBased => LISType.RegBased
      case other            => throw new IllegalArgumentException(s"Unsupported CFAR LIS type: $other")
    }

  private def parseEdgePolicy(value: String): Int =
    value match {
      case "SuppressEdges"    => CFAREdgePolicy.SuppressEdges
      case "OneSidedAverage"  => CFAREdgePolicy.OneSidedAverage
      case "WrapAroundFrame"  => CFAREdgePolicy.WrapAroundFrame
      case other              => throw new IllegalArgumentException(s"Unsupported CFAR edge policy: $other")
    }
}
