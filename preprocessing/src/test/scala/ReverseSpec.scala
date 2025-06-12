package opera.preprocessing

import chiseltest._
import chiseltest.formal._
import opera.common.TestAXI4StreamBlock
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class ReverseSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  implicit val p: Parameters = Parameters.empty

  val beatBytes  = 2
  val dataSize   = 128
  val dataRandom = true
  val random     = true
  val verbose    = false

  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  for (en <- Seq(true, false)) {
    "Reverse" should f"pass when enable = $en" in {
      val lazyDut = LazyModule(new Reverse with TestAXI4StreamBlock {
        override def dataBytes: Int = beatBytes
      })

      test(lazyDut.module)
        .withAnnotations(annotations)
        .runPeekPoke(
          _ => new ReverseTester(lazyDut, en, dataSize, dataRandom, beatBytes, random, verbose)
        )
    }

    "Reverse formal" should f"pass when enable = $en" taggedAs FormalTag in {
      verify(new ReverseWrapper(beatBytes, en), Seq(BoundedCheck(200), DefaultBackend))
    }
  }
}
