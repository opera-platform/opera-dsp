package opera.fft


import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.DspComplex
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class BitReverseSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "BitReverse"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val sampleSize  = 8
  val verbose     = true
  val random      = true

  val params = BitReverseParams.fixedPoint(
    dataType = DspComplex(FixedPoint(16.W, 14.BP)),
    memDepth = sampleSize,
    runTime = false,
    singlePortMem = true
  )

  it should "pass when: \n" +
    s"\t\tsampleSize          = $sampleSize, \n" in {

    test(new BitReverse[FixedPoint](params = params))
      .withAnnotations(annotations)
      .runPeekPoke(c =>
        new BitReverseTester(
          dut = c,
          params = params,
          sampleSize = sampleSize,
          verbose = verbose,
          random = random
        )
      )
  }
}
