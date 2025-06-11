package opera.preprocessing

import chiseltest._
import dsptools.numbers.implicits._
import freechips.rocketchip.diplomacy.AddressSet
import opera.common.TestStandaloneAXI4Block
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class PreProcessingSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "PreProcessing"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes = 4
  val silentFail = true
  val verbose = false
  val address = AddressSet(0x2000, 0xFF)

  val params = PreProcessingParameters(
    MaxChirpSize = 256,
    MaxChirpsPerFrame = 8,
    CrcParams = CRCParameters(
      dataBytes = beatBytes/2,
      polynomial = 0x04C11DB7,
      init = 0xFFFFFFFFL,
      reflectIn = false,
      reflectOut = false,
      xorOut = 0x00000000L
    ),
    BufferParams = BufferParameters(
      insertBuffers = false,
      size = 2
    )
  )

  for (samples <- createSubSequence(1 to params.MaxChirpSize, 3)) {
    for (samplesExpected <- createSubSequence(1 to samples, 3)) {
      for (format <- 0 to 3) {
        for (ctrl <- 0 to 15) {
          for (chirps <- createSubSequence(1 to params.MaxChirpsPerFrame, 3)) {
            val configuration = TestConfiguration(
              regs = Seq(
                RegConfiguration(
                  chirpsize = samples,
                  expectedsize = samplesExpected,
                  chirpperframe = chirps,
                  dataformat = format,
                  ctrl = ctrl
                )
              )
            )

            it should "pass when:\n" +
              s"\t\tnumber of samples = $samples,\n" +
              s"\t\texpected samples  = $samplesExpected,\n" +
              s"\t\tformat            = $format,\n" +
              s"\t\tctrl              = $ctrl,\n" +
              s"\t\tchirps            = $chirps\n" in {
              val lazyDut = LazyModule(new PreProcessingAXI4(address, params, beatBytes) with TestStandaloneAXI4Block {
                override def dataBytes: Int = beatBytes / 2
              })

              test(lazyDut.module)
                .withAnnotations(annotations)
                .runPeekPoke(
                  _ => new PreProcessingTester(lazyDut, address, params, configuration, beatBytes, silentFail, verbose)
                )
            }
          }
        }
      }
    }
  }
}
