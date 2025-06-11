package opera.preprocessing

import chiseltest._
import chiseltest.formal._
import opera.common.TestAXI4StreamBlock
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class CheckerCRCSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption {
  implicit val p: Parameters = Parameters.empty
  val maxSamples = 256
  val samplesExpected = 128
  val dataRandom = true
  val silentFail = true
  val verbose = false

  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  for (beatBytes <- Seq(1, 2, 4)) {
    for (init <- Seq(0xFFFFFFFFL, 0x00000000L)) {
      for (xorOut <- Seq(0xFFFFFFFFL, 0x00000000L)) {
        for (reflect <- Seq(false, true)) {
          val params = CRCParameters(
            dataBytes = beatBytes,
            polynomial = 0x04C11DB7,
            init = init,
            reflectIn = reflect,
            reflectOut = reflect,
            xorOut = xorOut
          )
          for (en <- Seq(true, false)) {
            "CheckerCRC" should "pass when:\n" +
              f"\t\tenable     = $en\n" +
              f"\t\tdataBytes  = ${params.dataBytes}\n" +
              f"\t\tpolynomial = 0x${params.polynomial}%08X\n" +
              f"\t\tinit       = 0x${params.init}%08X\n" +
              f"\t\treflectIn  = ${params.reflectIn}\n" +
              f"\t\treflectOut = ${params.reflectOut}\n" +
              f"\t\txorOut     = 0x${params.xorOut}%08X\n" in {
              val lazyDut = LazyModule(new CheckerCRC(params, maxSamples) with TestAXI4StreamBlock {
                override def dataBytes: Int = beatBytes
              })

              test(lazyDut.module)
                .withAnnotations(annotations)
                .runPeekPoke(
                  _ => new CheckerCRCTester(lazyDut, en, samplesExpected, params, dataRandom, silentFail, verbose)
                )
            }
          }
        }
      }
    }
  }
}
