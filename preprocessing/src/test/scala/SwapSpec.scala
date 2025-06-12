package opera.preprocessing

import chiseltest._
import chiseltest.formal._
import opera.common.TestAXI4StreamBlock
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class SwapSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  implicit val p: Parameters = Parameters.empty

  val beatBytes  = 4
  val dataSize   = 256
  val dataRandom = true
  val random     = true
  val verbose    = false

  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  for (format <- 0 to 3)
    for (en <- Seq(false, true)) {
      "Swap" should s"pass when enable = $en and format = $format" in {
        val lazyDut = LazyModule(new Swap(beatBytes) with TestAXI4StreamBlock {
          override def dataBytes: Int = beatBytes/2
        })

        test(lazyDut.module)
          .withAnnotations(annotations)
          .runPeekPoke(
            _ => new SwapTester(lazyDut, en, format, dataSize, dataRandom, beatBytes, random, verbose)
          )
      }

      "Swap formal" should s"pass when enable = $en and format = $format" taggedAs FormalTag in {
        verify(new SwapWrapper(beatBytes, en, format), Seq(BoundedCheck(200), DefaultBackend))
      }
  }
}
