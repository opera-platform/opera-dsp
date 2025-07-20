package opera.windowing

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, DspComplex, Floor, Round}
import fixedpoint._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.StandaloneTLBlock
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.scalatest.flatspec.AnyFlatSpec

class WindowingBinPointTLSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "WindowingTL"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes = 4
  val verbose   = false
  val random    = true

  val numPointsSeq         : Seq[Int]      = Seq(32)
  val inputBinPointSeq     : Seq[Int]      = Seq(8, 10, 12)
  val inputWholePartSeq    : Seq[Int]      = Seq(2, 3, 4)
  val outputBinOffsetSeq   : Seq[Int]      = Seq(0, 1, 2)
  val outWholePartOffsetSeq: Seq[Int]      = Seq(0, 2, 3)
  val coeffBinPointSeq     : Seq[Int]      = Seq(8, 10)
  val runTimeSeq           : Seq[Boolean]  = Seq(false)
  val windowEnableSeq      : Seq[Boolean]  = Seq(true)
  val constWindowSeq       : Seq[Boolean]  = Seq(true)
  val trimSeq              : Seq[TrimType] = Seq(Floor, Ceiling, Convergent, Round)
  def windowSeq(numPoints: Int): Seq[WindowType] = {
    Seq(TriangularWindow(numPoints, periodic = true))
  }

  val configs: Iterator[
    (
      Int,        // numPoints
      Int,        // inputBinPoint
      Int,        // inputWholePart
      Int,        // outputBinOffset
      Int,        // outWholePartOffset
      Int,        // coeffBinPoint
      WindowType, // window
      Boolean,    // runTime
      Boolean,    // windowEnable
      Boolean,    // constWindow
      TrimType    // trim
      )
  ] = for {
    numPoints          <- numPointsSeq.iterator
    inputBinPoint      <- inputBinPointSeq.iterator
    inputWholePart     <- inputWholePartSeq.iterator
    outputBinOffset    <- outputBinOffsetSeq.iterator
    outWholePartOffset <- outWholePartOffsetSeq.iterator
    coeffBinPoint      <- coeffBinPointSeq.iterator
    window             <- windowSeq(numPoints).iterator
    windowEnable       <- windowEnableSeq.iterator
    runTime            <- runTimeSeq.iterator
    constWindow        <- constWindowSeq.iterator
    trimType           <- trimSeq.iterator
  } yield (
    numPoints,
    inputBinPoint,
    inputWholePart,
    outputBinOffset,
    outWholePartOffset,
    coeffBinPoint,
    window,
    windowEnable,
    runTime,
    constWindow,
    trimType
  )

  configs.foreach {
    case (
      numPoints,
      inputBinPoint,
      inputWholePart,
      outputBinOffset,
      outWholePartOffset,
      coeffBinPoint,
      window,
      windowEnable,
      runTime,
      constWindow,
      trimType
    ) =>
      val outputBinPoint = outputBinOffset + inputBinPoint
      // Parameters
      val params = WindowingParams.fixed(
        inputType = DspComplex(FixedPoint((inputWholePart + inputBinPoint).W, inputBinPoint.BP)),
        outputType = DspComplex(FixedPoint((inputWholePart + outWholePartOffset + outputBinPoint).W, outputBinPoint.BP)),
        coeffType = FixedPoint((2 + coeffBinPoint).W, coeffBinPoint.BP),
        numPoints = numPoints,
        runTime = runTime,
        constWindow = constWindow,
        trimType = trimType,
        memoryFile = s"./test_run_dir/window_$numPoints.txt",
        windowFunc = window
      )
      // Data widths
      val inputWidth: Int = inputWholePart + inputBinPoint
      val outputWidth: Int = inputWholePart + outWholePartOffset + outputBinPoint
      val coeffWidth: Int = 2 + coeffBinPoint
      val inputBytes = math.ceil(inputWidth.toDouble / 4).toInt

      it should "pass when: \n" +
        s" winEn=$windowEnable,\n" +
        s" winType=${window.toString},\n" +
        s" winSize=$numPoints, \n" +
        s" constWin=$constWindow, \n" +
        s" runTime=$runTime, \n" +
        s" numPoints=$numPoints, \n" +
        s" input=($inputWidth, $inputBinPoint), \n" +
        s" output=($outputWidth, $outputBinPoint), \n" +
        s" coeff=($coeffWidth, $coeffBinPoint), \n" +
        s" trimType=$trimType, \n" in {
        val lazyDut = LazyModule(
          new WindowingTL[FixedPoint](
            csrAddress = AddressSet(0x60000 + numPoints * 4, 0xF),
            ramAddress = AddressSet(0x60000, numPoints * 4 - 1),
            params = params,
            beatBytes = beatBytes
          ) with StandaloneTLBlock {
            override def standaloneParams: TLBundleParameters =
              TLBundleParameters(
                addressBits = beatBytes * 8,
                dataBits = beatBytes * 8,
                sourceBits = 4,
                sinkBits = 1,
                sizeBits = 2,
                echoFields = Nil,
                requestFields = Nil,
                responseFields = Nil,
                hasBCE = false
              )
            override def dataBytes: Int = inputBytes
          }
        )

        test(lazyDut.module)
          .withAnnotations(annotations)
          .runPeekPoke(_ =>
            new WindowingTLTester(
              lazyDut,
              csrAddress = AddressSet(0x60000 + numPoints * 4, 0xF),
              ramAddress = AddressSet(0x60000, numPoints * 4 - 1),
              params = params,
              windowFuncRunTime = BlackmanWindow(numPoints / 2, periodic = true),
              beatBytes = beatBytes,
              enable = windowEnable,
              verbose = verbose,
              random = random
            )
          )
      }
  }
}
