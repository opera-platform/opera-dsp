package opera.windowing

import chisel3._
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation}
import dsptools.numbers.DspComplex
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class WindowingCoefficientEquivalenceSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Windowing coefficient quantization"

  implicit val p: Parameters = Parameters.empty
  private val window = CustomWindow("coefficient_ties_4.txt")

  Seq(true, false).foreach { constWindow =>
    it should s"produce the same tie-case values from ${if (constWindow) "ROM" else "RAM"}" in {
      val csrAddress = AddressSet(0x90000, 0xff)
      val ramAddress = AddressSet(0x80000, 0xff)
      val params = WindowingParams.fixed(
        inputType = DspComplex(FixedPoint(16.W, 4.BP)),
        outputType = DspComplex(FixedPoint(18.W, 4.BP)),
        coeffType = FixedPoint(8.W, 0.BP),
        numPoints = window.N,
        runTime = !constWindow,
        windowFunc = window,
        memoryFile = s"./test_run_dir/ties-${if (constWindow) "rom" else "ram"}.hex",
        constWindow = constWindow,
        mulPipeRegs = 1,
        roundPipeRegs = if (constWindow) 1 else 0,
        romStyle = if (constWindow) Synchronous else Distributed
      )
      val dut = WindowingTestDut.tl(csrAddress, ramAddress, params)
      test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(_ =>
        new WindowingTLTester(dut, csrAddress, ramAddress, window, params, 4,
          random = false, numFrames = 1, shortFirstFrame = !constWindow, seed = 0x544945L))
    }
  }
}
