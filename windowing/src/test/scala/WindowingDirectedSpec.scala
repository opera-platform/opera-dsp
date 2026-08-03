package opera.windowing

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation}
import dsptools.numbers.DspComplex
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class WindowingDirectedSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Windowing directed correctness"

  implicit val p: Parameters = Parameters.empty

  private def checkBypass(window: WindowType): Unit = {
    val beatBytes = 4
    val csrAddress = AddressSet(0x70000, 0xff)
    val ramAddress = AddressSet(0x71000, 0xff)
    val params = WindowingParams.fixed(
      inputType = DspComplex(FixedPoint(16.W, 12.BP)),
      outputType = DspComplex(FixedPoint(21.W, 14.BP)),
      coeffType = FixedPoint(16.W, 14.BP),
      numPoints = 8,
      runTime = false,
      windowFunc = window,
      constWindow = true
    )
    val dut = WindowingTestDut.tl(csrAddress, ramAddress, params, beatBytes)

    test(dut.module)
      .withAnnotations(Seq(VerilatorBackendAnnotation))
      .runPeekPoke(_ => new WindowingTLTester(
        dut,
        csrAddress,
        ramAddress,
        window,
        params,
        beatBytes,
        enable = window.function.isEmpty,
        random = false,
        numFrames = 1,
        seed = 0x425950415353L))
  }

  it should "preserve NoWindow values when output binary point increases" in {
    checkBypass(NoWindow())
  }

  it should "preserve disabled-window values when output binary point increases" in {
    checkBypass(BlackmanWindow(8, periodic = true))
  }
}
