package preprocessing

import chiseltest._
import chiseltest.formal._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class PadderSpec extends AnyFlatSpec with ChiselScalatestTester with Formal with FormalBackendOption with TestUtils {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val maxSamplesPerChirp = 16
  val maxChirpsPerFrame = 4
  val dataRandom = true
  val silentFail = true
  val verbose = false

  val annotations = Seq(WriteVcdAnnotation, TreadleBackendAnnotation)

  for (en <- Seq(false, true)) {
    for (samples <- createSubSequence(1 to maxSamplesPerChirp, 4)) {
      for (samplesExpected <- createSubSequence(1 to samples, 4)) {
        for (chirps <- createSubSequence(1 to maxChirpsPerFrame, 4)) {
          "Padder" should s"pass when:\n" +
            s"\t\tenable = $en,\n" +
            s"\t\tnumber of samples = $samples,\n" +
            s"\t\texpected samples = $samplesExpected,\n" +
            s"\t\tchirps = $chirps" in {
            val lazyDut = LazyModule(new Padder(maxSamplesPerChirp, maxChirpsPerFrame) with TestAXI4StreamBlock {
              override def dataBytes: Int = beatBytes
            })

            test(lazyDut.module)
              .withAnnotations(annotations)
              .runPeekPoke(
                _ => new PadderTester(lazyDut, en, samples, samplesExpected, chirps, dataRandom, beatBytes, silentFail, verbose)
              )
          }

          "Padder formal" should s"pass when:\n" +
            s"\t\tenable = $en,\n" +
            s"\t\tnumber of samples = $samples,\n" +
            s"\t\texpected samples = $samplesExpected,\n" +
            s"\t\tchirps = $chirps" taggedAs FormalTag in {
            verify(
              new PadderWrapper(en, maxSamplesPerChirp, maxChirpsPerFrame, samples, samplesExpected, chirps, beatBytes, verbose),
              annotations ++ Seq(BoundedCheck(maxSamplesPerChirp), DefaultBackend)
            )
          }
        }
      }
    }
  }
}
