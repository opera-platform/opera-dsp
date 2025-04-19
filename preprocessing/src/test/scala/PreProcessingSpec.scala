package preprocessing

import chiseltest._
import dsptools.numbers.implicits._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.scalatest.flatspec.AnyFlatSpec

class PreProcessingSpec extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = Parameters.empty

  val params = BlockParameters(
    ChirpSize = 32,
    QueueDepth = 4,
    MaxChirpsPerFrame = 4,
    UseBlockRam = false,
    GenLast = true
  )
  val beatBytes = 4
  val silentFail = true
  val address = AddressSet(0x2000,0xFF)
  val configuration = TestConfiguration(
    regs = Seq(
      RegConfiguration(
        chirpsize = 16,
        expectedsize = 12,
        chirpperframe = 1,
        dataformat = 0x2,
        ctrl = 0x8
      )
    )
  )

  val annotations = Seq(WriteVcdAnnotation, TreadleBackendAnnotation)
  it should "Test PreProcessing" in {
    val lazyDut = LazyModule(new PreProcessingAXI4(address, params, beatBytes) with PreProcessingAXI4Standalone)

    test(lazyDut.module)
      .withAnnotations(annotations)
      .runPeekPoke(
        _ => new PreProcessingTester(lazyDut, address, params, configuration, beatBytes, silentFail)
      )
  }
}
