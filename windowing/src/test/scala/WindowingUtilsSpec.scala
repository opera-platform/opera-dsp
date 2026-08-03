package opera.windowing

import chisel3.fromIntToWidth
import dsptools.numbers.DspComplex
import fixedpoint._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.io.Source
import scala.util.Using

class WindowingUtilsSpec extends AnyFlatSpec with Matchers {
  behavior of "Windowing utilities"

  private val parametersJson = Using.resource(
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("parameters.json"))
  )(_.mkString)

  private def configFile(json: String) = {
    val file = Files.createTempFile("windowing-parameters", ".json")
    Files.writeString(file, json)
    file
  }

  it should "round coefficient ties to even and mask signed hexadecimal values" in {
    WindowCoefficientQuantizer.quantize(0.5, width = 8, binaryPoint = 0) shouldBe BigInt(0)
    WindowCoefficientQuantizer.quantize(1.5, width = 8, binaryPoint = 0) shouldBe BigInt(2)
    WindowCoefficientQuantizer.quantize(2.5, width = 8, binaryPoint = 0) shouldBe BigInt(2)
    WindowCoefficientQuantizer.quantize(-1.5, width = 8, binaryPoint = 0) shouldBe BigInt(-2)
    WindowCoefficientQuantizer.toMaskedHex(BigInt(-1), width = 10) shouldBe "3FF"
    WindowCoefficientQuantizer.toMaskedHex(BigInt(1), width = 10) shouldBe "001"
  }

  it should "write nibble-padded two's-complement coefficient files" in {
    val file = Files.createTempFile("windowing-coefficients", ".hex")
    Utils.writeWindowFunction2File(
      fileName = file.toString,
      dataType = FixedPoint(10.W, 8.BP),
      window = Seq(0.0, 0.5, 1.0, -1.0 / 256)
    )
    Files.readAllLines(file, StandardCharsets.UTF_8).toArray.toSeq shouldBe
      Seq("000", "080", "100", "3FF")
  }

  it should "resolve custom windows from classpath resources" in {
    CustomWindow("custom_32.txt").N shouldBe 32
  }

  it should "reject missing, empty, malformed, negative, and oversized windows" in {
    an[IllegalArgumentException] should be thrownBy CustomWindow("does-not-exist.window")

    val empty = Files.createTempFile("windowing-empty", ".txt")
    an[IllegalArgumentException] should be thrownBy CustomWindow(empty.toString)

    val malformed = Files.createTempFile("windowing-malformed", ".txt")
    Files.writeString(malformed, "0.5\nnot-a-number\n")
    an[IllegalArgumentException] should be thrownBy CustomWindow(malformed.toString)

    an[IllegalArgumentException] should be thrownBy
      WindowCoefficientQuantizer.quantizeWindow(Seq(0.0, -0.25), width = 8, binaryPoint = 4)
    an[IllegalArgumentException] should be thrownBy
      WindowCoefficientQuantizer.quantizeWindow(Seq(8.0), width = 8, binaryPoint = 4)
  }

  it should "parse the shipped canonical configuration strictly" in {
    val parsed = ParseParameters.parseconfig(configFile(parametersJson).toString)
    parsed.isLeft shouldBe true
    val params = parsed.left.toOption.get._3
    params.numPoints shouldBe 256
    params.runTime shouldBe true
    params.constWindow shouldBe false
    params.mulPipeRegs shouldBe 0
    params.roundPipeRegs shouldBe 0
    params.romStyle shouldBe Distributed
  }

  it should "reject unknown and missing configuration values" in {
    def parseWith(rewrite: String => String): Throwable = {
      ParseParameters.parseconfig(configFile(rewrite(parametersJson)).toString).toOption.get
    }

    parseWith(_.replace("\"Convergent\"", "\"UnknownTrim\""))
      .getMessage should include("Unknown Windowing trimType")
    parseWith(_.replace("\"BlackmanWindow\"", "\"UnknownWindow\""))
      .getMessage should include("Unknown Windowing windowFunc")
    parseWith(_.replace("\"Distributed\"", "\"UnknownRom\""))
      .getMessage should include("Unknown Windowing romStyle")
    parseWith(_.replace("    \"runTime\"       : true,\n", ""))
      .getMessage should include("Missing required parameters field 'runTime'")
    parseWith(_.replace("\"runTime\"       : true", "\"runTime\"       : \"true\""))
      .getMessage should include("Invalid parameters field 'runTime'")
    parseWith(_.replace("\"mulPipeRegs\"   : 0", "\"mulPipeRegs\"   : 2"))
      .getMessage should include("mulPipeRegs must be 0 or 1")
    parseWith(_.replace("\"foldSymmetric\" : false", "\"foldSymmetric\" : true"))
      .getMessage should include("foldSymmetric requires")
    parseWith(_.replace(
      "    \"sigma\"         : 0.5,",
      "    \"sigma\"         : 0.5,\n    \"unexpected\"    : true,"))
      .getMessage should include("Unknown parameters key")
  }

  it should "reject invalid implementation and fixed-point configurations" in {
    def params(
      output: FixedPoint = FixedPoint(18.W, 14.BP),
      constWindow: Boolean = true,
      window: WindowType = BlackmanWindow(256, periodic = true),
      mul: Int = 0,
      round: Int = 0,
      style: RomStyle = Distributed,
      fold: Boolean = false,
      runTime: Boolean = false
    ): WindowingParams[FixedPoint] = WindowingParams.fixed(
      inputType = DspComplex(FixedPoint(16.W, 14.BP)),
      outputType = DspComplex(output),
      coeffType = FixedPoint(16.W, 14.BP),
      numPoints = 256,
      runTime = runTime,
      windowFunc = window,
      memoryFile = "",
      constWindow = constWindow,
      mulPipeRegs = mul,
      roundPipeRegs = round,
      romStyle = style,
      foldSymmetric = fold
    )

    an[IllegalArgumentException] should be thrownBy params(output = FixedPoint(18.W, 13.BP))
    an[IllegalArgumentException] should be thrownBy params(output = FixedPoint(15.W, 14.BP))
    an[IllegalArgumentException] should be thrownBy params(mul = 0, round = 1)
    an[IllegalArgumentException] should be thrownBy params(mul = 0, style = Synchronous)
    an[IllegalArgumentException] should be thrownBy params(constWindow = false, window = NoWindow())
    an[IllegalArgumentException] should be thrownBy
      params(mul = 1, style = Synchronous, fold = true, runTime = true)
  }

}
