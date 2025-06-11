package opera.preprocessing

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CRCSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  val sampleSize = 128

  for (init <- Seq(0xFFFFFFFFL, 0x00000000L)) {
    for (xorOut <- Seq(0xFFFFFFFFL, 0x00000000L)) {
      for (ref <- Seq(false, true)) {
        // CRC parameters
        val params = CRCParameters(
          dataBytes = 2,
          polynomial = 0x04C11DB7,
          init = init,
          reflectIn = ref,
          reflectOut = ref,
          xorOut = xorOut
        )
        // Generate input test vector
        val inputBytes = Array.fill(sampleSize)((scala.util.Random.nextInt(256) - 128).toByte)
        val input = inputBytes.grouped(params.dataBytes).toSeq.map(
          m => m.reverse.foldLeft(BigInt(0)) { (acc, b) => (acc << 8) | (b & 0xFF) }
        )

        "CRC" should "pass when:\n" +
          f"\t\tdataBytes  = ${params.dataBytes}\n" +
          f"\t\tpolynomial = 0x${params.polynomial}%08X\n" +
          f"\t\tinit       = 0x${params.init}%08X\n" +
          f"\t\treflectIn  = ${params.reflectIn}\n" +
          f"\t\treflectOut = ${params.reflectOut}\n" +
          f"\t\txorOut     = 0x${params.xorOut}%08X\n" in {
          test(new CRC(params)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
            val crcValue = crc32(inputBytes, params, 32)

            for (i <- input.indices) {
              c.io.i_data.poke(input(i).U)
              c.io.i_en.poke(true.B)
              c.clock.step(1)
            }
            c.io.i_en.poke(false.B)
            c.io.i_done.poke(true.B)
            c.io.o_crc.expect(crcValue.U)
            print(f"Expected value: 0x$crcValue%08X, peeked value: 0x${c.io.o_crc.peekInt()}%08X\n")
            c.clock.step(10)
          }
        }
      }
    }
  }
}
