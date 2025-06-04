package windowing

import chiseltest.{ChiselScalatestTester, TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}
import fixedpoint._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.TLBundleParameters
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.scalatest.flatspec.AnyFlatSpec

class WindowingTLSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Windowing-TL"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes = 4
  val verbose   = false
  val random    = true

  for (enable <- Seq(false, true)) {
    for (numPoints <- Seq(2048)) {
      for (window <- Seq(
        TriangularWindow(numPoints, periodic = true),
        HammingWindow(numPoints, periodic = true),
        HanningWindow(numPoints, periodic = true),
        BlackmanWindow(numPoints, periodic = true),
        GaussianWindow(numPoints, sigma = 0.5, periodic = true),
        CustomWindow(f"src/main/resources/custom_$numPoints.txt"),
        NoWindow()
      )) {
        for (constWindow <- Seq(false, true)) {
          for (runTime <- Seq(false, true)) {
            for (trimType <- Seq(Floor, Ceiling, Convergent, Round)) {
              for (binPoint <- Seq(10, 12, 14)) {
                // Address
                val ramAddress = AddressSet(0x60000, numPoints * 4 - 1)
                val csrAddress = AddressSet(0x60000 + numPoints * 4, 0xF)
                // Parameters
                val params = WindowingParams.fixed(
                  numPoints   = numPoints,
                  dataWidth   = binPoint + 2,
                  binPoint    = binPoint,
                  runTime     = runTime,
                  constWindow = constWindow,
                  trimType    = trimType,
                  memoryFile  = s"./test_run_dir/window_$numPoints.txt",
                  windowFunc  = window
                )

                it should "pass when: \n" +
                  s"\t\twindow enabled = $enable,\n" +
                  s"\t\twindow type    = ${window.toString},\n" +
                  s"\t\twindow size    = $numPoints,\n" +
                  s"\t\tconst window   = $constWindow,\n" +
                  s"\t\trun-time       = $runTime,\n" +
                  s"\t\ttrim type      = $trimType,\n" +
                  s"\t\tbinary point   = $binPoint\n" in {
                  val lazyDut = LazyModule(
                    new WindowingTL[FixedPoint](
                      csrAddress = csrAddress,
                      ramAddress = ramAddress,
                      params = params,
                      beatBytes = beatBytes
                    ) with TestStandaloneTLBlock {
                      override def standaloneParams: TLBundleParameters =
                        TLBundleParameters(
                          addressBits    = beatBytes * 8,
                          dataBits       = beatBytes * 8,
                          sourceBits     = 4,
                          sinkBits       = 1,
                          sizeBits       = 2,
                          echoFields     = Nil,
                          requestFields  = Nil,
                          responseFields = Nil,
                          hasBCE         = false
                        )

                      override def dataBytes: Int = 4
                    }
                  )

                  test(lazyDut.module)
                    .withAnnotations(annotations)
                    .runPeekPoke(_ =>
                      new WindowingTLTester(
                        lazyDut,
                        csrAddress = csrAddress,
                        ramAddress = ramAddress,
                        params = params,
                        windowFuncRunTime = BlackmanWindow(numPoints / 2, periodic = true),
                        beatBytes = beatBytes,
                        enable = enable,
                        verbose = verbose,
                        random = random
                      )
                    )
                }
              }
            }
          }
        }
      }
    }
  }
}
