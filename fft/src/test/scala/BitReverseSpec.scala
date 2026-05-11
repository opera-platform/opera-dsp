package opera.fft

import chisel3.fromIntToWidth
import chiseltest.ChiselScalatestTester
import dsptools.numbers.DspComplex
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Checks bit-reversed frame ordering for register and SRAM-backed BitReverse RTL.
 */
class BitReverseSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of "BitReverse"

  implicit val p: Parameters = Parameters.empty
  def annotations = TestConfig.annotations

  private case class BitReverseCase(
      memDepth     : Int,
      frameSize    : Int,
      runTime      : Boolean,
      singlePortMem: Boolean,
  )

  private val memDepthSeq = Seq(4, 8, 16)
  private val runTimeSeq = Seq(false, true)
  private val singlePortMemSeq = Seq(false, true)

  private def frameSizeSeq(memDepth: Int, runTime: Boolean): Seq[Int] =
    if (runTime) Seq(memDepth, memDepth / 2) else Seq(memDepth)

  private val configs: Iterator[BitReverseCase] = for {
    memDepth      <- memDepthSeq.iterator
    runTime       <- runTimeSeq.iterator
    frameSize     <- frameSizeSeq(memDepth, runTime).iterator
    singlePortMem <- singlePortMemSeq.iterator
  } yield BitReverseCase(memDepth, frameSize, runTime, singlePortMem)

  // Checks bit-reversed output order by streaming deterministic frames through register and SRAM memory variants.
  configs.foreach { config =>
    val params = BitReverseParams(
      dataType = DspComplex(FixedPoint(16.W, 14.BP)),
      memDepth = config.memDepth,
      runTime = config.runTime,
      singlePortMem = config.singlePortMem
    )

    it should TestUtils.passWhen(
      "memDepth" -> config.memDepth,
      "frameSize" -> config.frameSize,
      "runTime" -> config.runTime,
      "singlePortMem" -> config.singlePortMem,
    ) in {
      test(new BitReverse(params = params))
        .withAnnotations(annotations)
        .runPeekPoke(c =>
          new BitReverseTester(
            dut = c,
            frameSize = config.frameSize
          )
        )
    }
  }
}
