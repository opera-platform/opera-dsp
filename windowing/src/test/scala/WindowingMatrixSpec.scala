package opera.windowing

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, DspComplex, Floor, Round}
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

case class WindowingTestConfig(
    numPoints: Int,
    inputBinPoint: Int,
    inputWholePart: Int,
    outputBinOffset: Int,
    outputWholePartOffset: Int,
    coeffBinPoint: Int,
    window: WindowType,
    windowEnable: Boolean,
    runTime: Boolean,
    constWindow: Boolean,
    trimType: TrimType)

case class WindowingTestAxes(
    numPoints: Seq[Int],
    inputBinPoints: Seq[Int],
    inputWholeParts: Seq[Int],
    outputBinOffsets: Seq[Int],
    outputWholePartOffsets: Seq[Int],
    coeffBinPoints: Seq[Int],
    windows: Int => Seq[WindowType],
    runTimes: Seq[Boolean],
    windowEnables: Seq[Boolean],
    constWindows: Seq[Boolean],
    trims: Seq[TrimType])

object WindowingTestMatrix {
  private def configurations(axes: WindowingTestAxes): Seq[WindowingTestConfig] = {
    for {
      numPoints <- axes.numPoints
      inputBinPoint <- axes.inputBinPoints
      inputWholePart <- axes.inputWholeParts
      outputBinOffset <- axes.outputBinOffsets
      outputWholePartOffset <- axes.outputWholePartOffsets
      coeffBinPoint <- axes.coeffBinPoints
      window <- axes.windows(numPoints)
      windowEnable <- axes.windowEnables
      runTime <- axes.runTimes
      constWindow <- axes.constWindows
      if window.function.nonEmpty || constWindow
      trimType <- axes.trims
    } yield WindowingTestConfig(
      numPoints,
      inputBinPoint,
      inputWholePart,
      outputBinOffset,
      outputWholePartOffset,
      coeffBinPoint,
      window,
      windowEnable,
      runTime,
      constWindow,
      trimType)
  }

  val binPoint: Seq[WindowingTestConfig] = configurations(WindowingTestAxes(
    numPoints = Seq(32),
    inputBinPoints = Seq(8, 10, 12),
    inputWholeParts = Seq(2, 3, 4),
    outputBinOffsets = Seq(0, 1, 2),
    outputWholePartOffsets = Seq(0, 2, 3),
    coeffBinPoints = Seq(8, 10),
    windows = size => Seq(TriangularWindow(size, periodic = true)),
    runTimes = Seq(false),
    windowEnables = Seq(true),
    constWindows = Seq(true),
    trims = Seq(Floor, Ceiling, Convergent, Round)))

  val functions: Seq[WindowingTestConfig] = configurations(WindowingTestAxes(
    numPoints = Seq(32, 256, 2048),
    inputBinPoints = Seq(10),
    inputWholeParts = Seq(2),
    outputBinOffsets = Seq(0),
    outputWholePartOffsets = Seq(2),
    coeffBinPoints = Seq(10),
    windows = size => Seq(
      TriangularWindow(size, periodic = true),
      HammingWindow(size, periodic = true),
      HanningWindow(size, periodic = true),
      BlackmanWindow(size, periodic = true),
      GaussianWindow(size, sigma = 0.5, periodic = true),
      CustomWindow(s"custom_$size.txt"),
      NoWindow()),
    runTimes = Seq(false, true),
    windowEnables = Seq(false, true),
    constWindows = Seq(false, true),
    trims = Seq(Floor, Ceiling, Convergent, Round)))
}

abstract class WindowingMatrixSpec(
    protocolName: String,
    memoryFile: WindowingTestConfig => String,
    configs: Seq[WindowingTestConfig],
    expectedCases: Int)
  extends AnyFlatSpec
  with ChiselScalatestTester {

  behavior of protocolName

  protected implicit val p: Parameters = Parameters.empty
  protected val beatBytes = 4
  protected val annotations = Seq(VerilatorBackendAnnotation) ++
    (if (sys.env.contains("WIN_VCD")) Seq(WriteVcdAnnotation) else Nil)

  require(configs.size == expectedCases,
    s"$protocolName matrix contains ${configs.size} cases instead of $expectedCases")

  protected def runCase(
      config: WindowingTestConfig,
      params: WindowingParams[FixedPoint],
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      inputBytes: Int): Unit

  configs.foreach { config =>
    val outputBinPoint = config.outputBinOffset + config.inputBinPoint
    // Parameters
    val params = WindowingParams.fixed(
      inputType = DspComplex(FixedPoint(
        (config.inputWholePart + config.inputBinPoint).W,
        config.inputBinPoint.BP)),
      outputType = DspComplex(FixedPoint(
        (config.inputWholePart + config.outputWholePartOffset + outputBinPoint).W,
        outputBinPoint.BP)),
      coeffType = FixedPoint((2 + config.coeffBinPoint).W, config.coeffBinPoint.BP),
      numPoints = config.numPoints,
      runTime = config.runTime,
      constWindow = config.constWindow,
      trimType = config.trimType,
      memoryFile = s"./test_run_dir/${memoryFile(config)}",
      windowFunc = config.window)

    // Data widths
    val inputWidth = config.inputWholePart + config.inputBinPoint
    val outputWidth = config.inputWholePart + config.outputWholePartOffset + outputBinPoint
    val coeffWidth = 2 + config.coeffBinPoint
    val inputBytes = math.ceil(inputWidth.toDouble / 4).toInt
    val csrAddress = AddressSet(0x60000 + config.numPoints * 4, 0xF)
    val ramAddress = AddressSet(0x60000, config.numPoints * 4 - 1)

    it should "pass when: \n" +
      s" winEn=${config.windowEnable},\n" +
      s" winType=${config.window},\n" +
      s" winSize=${config.numPoints}, \n" +
      s" constWin=${config.constWindow}, \n" +
      s" runTime=${config.runTime}, \n" +
      s" numPoints=${config.numPoints}, \n" +
      s" input=($inputWidth, ${config.inputBinPoint}), \n" +
      s" output=($outputWidth, $outputBinPoint), \n" +
      s" coeff=($coeffWidth, ${config.coeffBinPoint}), \n" +
      s" trimType=${config.trimType}, \n" in {
      runCase(config, params, csrAddress, ramAddress, inputBytes)
    }
  }
}

abstract class WindowingTLMatrixSpec(
    memoryFile: WindowingTestConfig => String,
    configs: Seq[WindowingTestConfig],
    expectedCases: Int)
  extends WindowingMatrixSpec("WindowingTL", memoryFile, configs, expectedCases) {

  override protected def runCase(
      config: WindowingTestConfig,
      params: WindowingParams[FixedPoint],
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      inputBytes: Int): Unit = {
    val dut = WindowingTestDut.tl(
      csrAddress, ramAddress, params, beatBytes, streamBytes = inputBytes)

    test(dut.module).withAnnotations(annotations).runPeekPoke(_ =>
      new WindowingTLTester(
        dut,
        csrAddress,
        ramAddress,
        BlackmanWindow(config.numPoints / 2, periodic = true),
        params,
        beatBytes,
        enable = config.windowEnable))
  }
}

abstract class WindowingAXI4MatrixSpec(
    memoryFile: WindowingTestConfig => String,
    configs: Seq[WindowingTestConfig],
    expectedCases: Int)
  extends WindowingMatrixSpec("WindowingAXI4", memoryFile, configs, expectedCases) {

  override protected def runCase(
      config: WindowingTestConfig,
      params: WindowingParams[FixedPoint],
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      inputBytes: Int): Unit = {
    val dut = WindowingTestDut.axi4(
      csrAddress, ramAddress, params, beatBytes, streamBytes = inputBytes)

    test(dut.module).withAnnotations(annotations).runPeekPoke(_ =>
      new WindowingAXI4Tester(
        dut,
        csrAddress,
        ramAddress,
        BlackmanWindow(config.numPoints / 2, periodic = true),
        params,
        beatBytes,
        enable = config.windowEnable))
  }
}

class WindowingBinPointTLSpec
  extends WindowingTLMatrixSpec(
    config => s"window_binpoint_tl_${config.numPoints}_q${config.coeffBinPoint}.txt",
    WindowingTestMatrix.binPoint,
    648)

class WindowingFunctionsTLSpec
  extends WindowingTLMatrixSpec(
    config => s"window_functions_tl_${config.window}_q${config.coeffBinPoint}.txt",
    WindowingTestMatrix.functions,
    624)

class WindowingBinPointAXI4Spec
  extends WindowingAXI4MatrixSpec(
    config => s"window_binpoint_axi4_${config.numPoints}_q${config.coeffBinPoint}.txt",
    WindowingTestMatrix.binPoint,
    648)

class WindowingFunctionsAXI4Spec
  extends WindowingAXI4MatrixSpec(
    config => s"window_functions_axi4_${config.window}_q${config.coeffBinPoint}.txt",
    WindowingTestMatrix.functions,
    624)
