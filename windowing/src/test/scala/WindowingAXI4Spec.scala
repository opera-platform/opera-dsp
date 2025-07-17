package opera.windowing

import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}
import fixedpoint._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import opera.common.TestStandaloneAXI4Block
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.scalatest.flatspec.AnyFlatSpec

class WindowingAXI4Spec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "WindowingAXI4"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes = 4
  val verbose   = true
  val random    = true

  // Iterators for test
  val boolSeq    : Seq[Boolean]  = Seq(false, true)
  val pointsSeq  : Seq[Int]      = Seq(32, 256, 2048)
  val trimSeq    : Seq[TrimType] = Seq(Floor, Ceiling, Convergent, Round)
  val binPointSeq: Seq[Int]      = Seq(14)
  def windowSeq(numPoints: Int): Seq[WindowType] = {
    Seq(
      TriangularWindow(numPoints, periodic = true),
      HammingWindow(numPoints, periodic = true),
      HanningWindow(numPoints, periodic = true),
      BlackmanWindow(numPoints, periodic = true),
      GaussianWindow(numPoints, sigma = 0.5, periodic = true),
      CustomWindow(f"src/main/resources/custom_$numPoints.txt"),
      NoWindow()
    )
  }

  val configs: Iterator[(WindowingParams[FixedPoint], AddressSet, AddressSet, Boolean)] = for {
    numPoints   <- pointsSeq.iterator
    binPoint    <- binPointSeq.iterator
    runTime     <- boolSeq.iterator
    constWindow <- boolSeq.iterator
    trimType    <- trimSeq.iterator
    window      <- windowSeq(numPoints).iterator
    enable      <- boolSeq.iterator
  } yield (
    WindowingParams.fixed(
      numPoints = numPoints,
      dataWidth = binPoint + 2,
      binPoint = binPoint,
      runTime = runTime,
      constWindow = constWindow,
      trimType = trimType,
      memoryFile = s"./test_run_dir/window_$numPoints.txt",
      windowFunc = window
    ),
    AddressSet(0x60000 + numPoints * 4, 0xF),
    AddressSet(0x60000, numPoints * 4 - 1),
    enable
   )

  configs.foreach { config =>
    it should "pass when: \n" +
      s"  window enabled = ${config._4},\n" +
      s"  window type    = ${config._1.windowFunc.toString},\n" +
      s"  window size    = ${config._1.numPoints}, \n" +
      s"  const window   = ${config._1.constWindow}, \n" +
      s"  run-time       = ${config._1.runTime}, \n" +
      s"  trim type      = ${config._1.trimType}, \n" in {
      val lazyDut = LazyModule(
        new WindowingAXI4[FixedPoint](
          csrAddress = config._2,
          ramAddress = config._3,
          errAddress = Nil,
          params     = config._1,
          beatBytes  = beatBytes
        ) with TestStandaloneAXI4Block {
          override def standaloneParams: AXI4BundleParameters = AXI4BundleParameters(addrBits = beatBytes * 8, dataBits = beatBytes * 8, idBits = 1)
          override def dataBytes: Int = 4
        }
      )

      test(lazyDut.module)
        .withAnnotations(annotations)
        .runPeekPoke(_ =>
          new WindowingAXI4Tester(
            lazyDut,
            csrAddress = config._2,
            ramAddress = config._3,
            params = config._1,
            windowFuncRunTime = BlackmanWindow(config._1.numPoints / 2, periodic = true),
            beatBytes = beatBytes,
            enable = {config._4},
            verbose = verbose,
            random = random
          )
        )
    }
  }
}
