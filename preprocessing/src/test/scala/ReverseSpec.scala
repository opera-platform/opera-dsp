package preprocessing

import chiseltest._
import chiseltest.formal._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class ReverseSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 2
  val dataSize = 8
  val dataRandom = false
  val silentFail = true
  val verbose = true

  val annotations = Seq(WriteVcdAnnotation, TreadleBackendAnnotation)

  "Reverse" should "pass when reverse is enabled" in {
    val lazyDut = LazyModule(new Reverse with TestAXI4StreamBlock {
      override def dataBytes: Int = beatBytes
    })

    test(lazyDut.module)
      .withAnnotations(annotations)
      .runPeekPoke(
        _ => new ReverseTester(lazyDut, en = true, dataSize, dataRandom, beatBytes, silentFail, verbose)
      )
  }

  "Reverse" should "pass when reverse is disabled" in {
    val lazyDut = LazyModule(new Reverse with TestAXI4StreamBlock {
      override def dataBytes: Int = beatBytes
    })

    test(lazyDut.module)
      .withAnnotations(annotations)
      .runPeekPoke(
        _ => new ReverseTester(lazyDut, en = false, dataSize, dataRandom, beatBytes, silentFail, verbose)
      )
  }

  "Reverse formal" should "pass when reverse is enabled" taggedAs FormalTag in {
    verify(new ReverseWrapper(beatBytes, en = true), Seq(BoundedCheck(200), DefaultBackend))
  }

  "Reverse formal" should "pass when reverse is disabled" taggedAs FormalTag in {
    verify(new ReverseWrapper(beatBytes, en = false), Seq(BoundedCheck(200), DefaultBackend))
  }
}
