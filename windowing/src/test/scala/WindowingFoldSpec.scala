package opera.windowing

import chisel3._
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation}
import dsptools.numbers.DspComplex
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class WindowingFoldSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Windowing folded ROM"

  implicit val p: Parameters = Parameters.empty
  private val csrAddress = AddressSet(0x90000, 0xff)
  private val ramAddress = AddressSet(0x80000, 0x3ff)

  Seq(32, 256).foreach { size =>
    Seq(true, false).foreach { periodic =>
      val kind = if (periodic) "periodic" else "symmetric"
      it should s"match the full $kind Blackman-$size table" in {
        val window = BlackmanWindow(size, periodic)
        val params = WindowingParams.fixed(
          inputType = DspComplex(FixedPoint(16.W, 14.BP)),
          outputType = DspComplex(FixedPoint(18.W, 14.BP)),
          coeffType = FixedPoint(16.W, 14.BP),
          numPoints = size,
          runTime = false,
          windowFunc = window,
          memoryFile = s"./test_run_dir/fold-$size-$periodic.hex",
          constWindow = true,
          mulPipeRegs = 1,
          romStyle = Synchronous,
          foldSymmetric = true
        )
        val dut = WindowingTestDut.tl(csrAddress, ramAddress, params)
        test(dut.module).withAnnotations(Seq(VerilatorBackendAnnotation)).runPeekPoke(_ =>
          new WindowingTLTester(dut, csrAddress, ramAddress, window, params, 4,
            numFrames = 1, checkDisabled = true, seed = size * 2L + periodic.hashCode))
      }
    }
  }
}
