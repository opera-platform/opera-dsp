package opera.windowing

import chisel3.{Data, fromIntToWidth}
import chisel3.util.log2Ceil
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, DspComplex, Floor, Round}
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}
import freechips.rocketchip.diplomacy.AddressSet
import play.api.libs.json.{JsObject, JsValue, Json, Reads}

import java.io.{BufferedWriter, File, FileWriter}
import java.math.RoundingMode
import scala.util.{Try, Using}

object WindowCoefficientQuantizer {
  def quantize(value: Double, width: Int, binaryPoint: Int): BigInt = {
    require(value.isFinite, s"Window coefficient must be finite, got $value")
    require(width > 0, s"Coefficient width must be positive, got $width")
    require(binaryPoint >= 0, s"Coefficient binary point must be non-negative, got $binaryPoint")
    val scaled = BigDecimal.decimal(value) * BigDecimal(BigInt(1) << binaryPoint)
    val rounded = BigInt(scaled.bigDecimal.setScale(0, RoundingMode.HALF_EVEN).toBigIntegerExact)
    val minimum = -(BigInt(1) << (width - 1))
    val maximum = (BigInt(1) << (width - 1)) - 1
    require(rounded >= minimum && rounded <= maximum,
      s"Window coefficient $value quantizes to $rounded outside signed $width-bit range " +
        s"[$minimum, $maximum]")
    rounded
  }

  def quantizeWindow(window: Seq[Double], width: Int, binaryPoint: Int): Seq[BigInt] = {
    require(window.nonEmpty, "Window coefficient sequence must not be empty")
    val quantized = window.map(quantize(_, width, binaryPoint))
    require(!quantized.exists(_ < 0),
      "Negative window coefficients are not supported by the Windowing datapath")
    quantized
  }

  def toMaskedHex(value: BigInt, width: Int): String = {
    require(width > 0, s"Coefficient width must be positive, got $width")
    val mask = (BigInt(1) << width) - 1
    val digits = (width + 3) / 4
    val encoded = (value & mask).toString(16).toUpperCase
    "0" * (digits - encoded.length) + encoded
  }

  def isSymmetric(window: Seq[Double], width: Int, binaryPoint: Int, periodic: Boolean): Boolean = {
    val values = quantizeWindow(window, width, binaryPoint)
    values.indices.forall { index =>
      val mirror = if (periodic) {
        (values.length - index) % values.length
      } else {
        values.length - 1 - index
      }
      values(index) == values(mirror)
    }
  }
}

object Utils {
  private[windowing] def binaryPointOf(data: Data): Int = data match {
    case fixed: FixedPoint => fixed.binaryPoint.get
    case _ => 0
  }

  def addressMaskBits(address: AddressSet, beatBytes: Int): List[Boolean] = {
    val shiftedMask = address.mask >> log2Ceil(beatBytes)
    if (shiftedMask == 0) Nil else shiftedMask.toString(2).map(_ == '1').toList
  }

  def writeWindowFunction2File(
      fileName: String,
      dataType: Data,
      window: Seq[Double],
      dataPerWord: Int = 1
  ): Unit = {
    val width = dataType.getWidth
    val binaryPoint = binaryPointOf(dataType)
    require(dataPerWord > 0, s"dataPerWord must be positive, got $dataPerWord")
    require(window.nonEmpty, "Window coefficient sequence must not be empty")
    val encoded = window.map(WindowCoefficientQuantizer.quantize(_, width, binaryPoint))
      .map(WindowCoefficientQuantizer.toMaskedHex(_, width))

    val file = new File(fileName)
    Option(file.getParentFile).foreach(_.mkdirs())
    Using.resource(new BufferedWriter(new FileWriter(file))) { writer =>
      encoded.grouped(dataPerWord).foreach { word =>
        writer.write(word.mkString)
        writer.newLine()
      }
    }
  }
}

object ParseParameters {
  type Parsed = (AddressSet, AddressSet, WindowingParams[FixedPoint])

  private val requiredTopKeys = Set("csrAddress", "ramAddress", "parameters")
  private val requiredAddressKeys = Set("base", "mask")
  private val requiredParameterKeys = Set(
    "inputWidth", "inputBinPoint", "outputWidth", "outputBinPoint", "coeffWidth",
    "coeffBinPoint", "numPoints", "runTime", "constWindow", "trimType", "memoryFile",
    "windowFunc", "customFile", "periodic", "sigma"
  )
  private val optionalParameterKeys =
    Set("mulPipeRegs", "roundPipeRegs", "romStyle", "foldSymmetric")

  private def obj(value: JsValue, context: String): JsObject = value match {
    case value: JsObject => value
    case _ => throw new IllegalArgumentException(s"$context must be a JSON object")
  }

  private def rejectUnknown(value: JsObject, allowed: Set[String], context: String): Unit = {
    val unknown = value.keys.diff(allowed)
    require(unknown.isEmpty, s"Unknown $context key(s): ${unknown.toSeq.sorted.mkString(", ")}")
  }

  private def required[T: Reads](value: JsObject, key: String, context: String): T =
    value.value.get(key) match {
      case None => throw new IllegalArgumentException(s"Missing required $context field '$key'")
      case Some(raw) => raw.validate[T].fold(
        errors => throw new IllegalArgumentException(
          s"Invalid $context field '$key': ${errors.mkString(", ")}"),
        identity
      )
    }

  private def optional[T: Reads](value: JsObject, key: String, default: T, context: String): T =
    value.value.get(key).map(_.validate[T].fold(
      errors => throw new IllegalArgumentException(
        s"Invalid $context field '$key': ${errors.mkString(", ")}"),
      identity
    )).getOrElse(default)

  private def parseAddress(value: JsObject, context: String): AddressSet = {
    rejectUnknown(value, requiredAddressKeys, context)
    def hex(key: String): BigInt = {
      val raw = required[String](value, key, context).stripSuffix("L")
      require(raw.startsWith("0x") || raw.startsWith("0X"),
        s"$context.$key must use 0x hexadecimal form")
      BigInt(raw.drop(2), 16)
    }
    AddressSet(hex("base"), hex("mask"))
  }

  def parseconfig(filename: String): Either[Parsed, Throwable] = {
    Try {
      val content = Using.resource(scala.io.Source.fromFile(filename)) { resource =>
        Json.parse(resource.mkString)
      }
      val top = obj(content, "configuration")
      rejectUnknown(top, requiredTopKeys, "configuration")
      val csrAddress = parseAddress(
        obj(required[JsValue](top, "csrAddress", "configuration"), "csrAddress"),
        "csrAddress")
      val ramAddress = parseAddress(
        obj(required[JsValue](top, "ramAddress", "configuration"), "ramAddress"),
        "ramAddress")
      val parametersJson = obj(required[JsValue](top, "parameters", "configuration"), "parameters")
      rejectUnknown(parametersJson, requiredParameterKeys ++ optionalParameterKeys, "parameters")

      val periodic = required[Boolean](parametersJson, "periodic", "parameters")
      val numPoints = required[Int](parametersJson, "numPoints", "parameters")
      val customFile = required[String](parametersJson, "customFile", "parameters")
      val sigma = required[Double](parametersJson, "sigma", "parameters")
      val trimType: TrimType = required[String](parametersJson, "trimType", "parameters") match {
        case "Floor" => Floor
        case "Ceiling" => Ceiling
        case "Convergent" => Convergent
        case "Round" => Round
        case invalid => throw new IllegalArgumentException(s"Unknown Windowing trimType '$invalid'")
      }
      val romStyle = optional[String](
        parametersJson, "romStyle", Distributed.jsonName, "parameters") match {
        case Distributed.jsonName => Distributed
        case Synchronous.jsonName => Synchronous
        case invalid => throw new IllegalArgumentException(s"Unknown Windowing romStyle '$invalid'")
      }
      val windowFunc: WindowType =
        required[String](parametersJson, "windowFunc", "parameters") match {
        case "TriangularWindow" => TriangularWindow(numPoints, periodic)
        case "HammingWindow" => HammingWindow(numPoints, periodic)
        case "HanningWindow" => HanningWindow(numPoints, periodic)
        case "BlackmanWindow" => BlackmanWindow(numPoints, periodic)
        case "GaussianWindow" => GaussianWindow(numPoints, sigma, periodic)
        case "NoWindow" => NoWindow()
        case "CustomWindow" => CustomWindow(customFile)
        case invalid =>
          throw new IllegalArgumentException(s"Unknown Windowing windowFunc '$invalid'")
      }

      val parameters = WindowingParams.fixed(
        inputType = DspComplex(FixedPoint(
          required[Int](parametersJson, "inputWidth", "parameters").W,
          required[Int](parametersJson, "inputBinPoint", "parameters").BP)),
        outputType = DspComplex(FixedPoint(
          required[Int](parametersJson, "outputWidth", "parameters").W,
          required[Int](parametersJson, "outputBinPoint", "parameters").BP)),
        coeffType = FixedPoint(
          required[Int](parametersJson, "coeffWidth", "parameters").W,
          required[Int](parametersJson, "coeffBinPoint", "parameters").BP),
        numPoints = numPoints,
        runTime = required[Boolean](parametersJson, "runTime", "parameters"),
        constWindow = required[Boolean](parametersJson, "constWindow", "parameters"),
        trimType = trimType,
        memoryFile = required[String](parametersJson, "memoryFile", "parameters"),
        windowFunc = windowFunc,
        mulPipeRegs = optional[Int](parametersJson, "mulPipeRegs", 0, "parameters"),
        roundPipeRegs = optional[Int](parametersJson, "roundPipeRegs", 0, "parameters"),
        romStyle = romStyle,
        foldSymmetric = optional[Boolean](parametersJson, "foldSymmetric", false, "parameters")
      )
      (csrAddress, ramAddress, parameters)
    }.fold(Right(_), Left(_))
  }
}
