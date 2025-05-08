package preprocessing

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.diplomacy.lazymodule._

class CheckerCRCTester
(
  dut: CheckerCRC with TestAXI4StreamBlock,
  en: Boolean = true,
  samplesExpected: Int,
  params: CRCParameters,
  dataRandom: Boolean = false,
  silentFail: Boolean = false,
  verbose: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel[LazyModuleImp] with TestUtils {

  if (verbose) {
    print(f"\n#################################################\n")
    print(f"# CheckerCRC options: \n")
    print(f"# \tenable          = $en\n")
    print(f"# \tsamplesExpected = $samplesExpected\n")
    print(f"# \tdataRandom      = $dataRandom\n")
    print(f"# \tCRC parameters: \n")
    print(f"# \t\tdataBytes  = ${params.dataBytes}\n")
    print(f"# \t\tpolynomial = 0x${params.polynomial}%08X\n")
    print(f"# \t\tinit       = 0x${params.init}%08X\n")
    print(f"# \t\treflectIn  = ${params.reflectIn}\n")
    print(f"# \t\treflectOut = ${params.reflectOut}\n")
    print(f"# \t\txorOut     = 0x${params.xorOut}%08X\n")
    print(f"#################################################\n")
  }

  val mod: LazyModuleImp = dut.module
  // Bind nodes
  val inMaster: AXI4StreamPeekPokeMaster = bindMaster(dut.in.getWrappedValue)

  // Reset stream nodes
  resetMaster(dut.in)
  resetSlave(dut.out)
  step(1)
  poke(dut.io.i_en, en)
  poke(dut.io.i_samples_expected, samplesExpected.U)
  step(1)

  poke(dut.out.ready, true.B)
  step(1)
  // generate test array
  val inputBytes: Array[Byte] = Array.fill(samplesExpected*params.dataBytes)((scala.util.Random.nextInt(256) - 128).toByte)
  val inData: Seq[BigInt] = inputBytes.grouped(params.dataBytes).toSeq.map(
    m => m.reverse.foldLeft(BigInt(0)) { (acc, b) => (acc << 8) | (b & 0xFF) }
  )

  // Generate output of the model
  val crcValue: BigInt = BigInt(crc32(inputBytes, params, 32))
  val inCRC: Seq[BigInt] = for (i <- 0 until 4/params.dataBytes) yield {
    (crcValue >> (i * params.dataBytes * 8)) & ((BigInt(1) << (params.dataBytes * 8)) - 1)
  }

  // Add CRC to the data if CRC enabled
  val inDataWithCRC: Seq[BigInt] = if (en) inData ++ inCRC else inData

  // Add transactions
  inMaster.addTransactions(inDataWithCRC.map { m => AXI4StreamTransaction(data = m) })

  // Counters to keep track of read and written data
  var read_counter = 0
  var write_counter = 0
  // Peeked values
  var peekedValue: BigInt = 0

  // Check output data
  while (read_counter < inData.length && write_counter <= inDataWithCRC.length) {
    // Randomize ready and valid
    poke(dut.in.valid,  scala.util.Random.nextInt(2))
    poke(dut.out.ready, scala.util.Random.nextInt(2))

    // Keep track of written data
    if (peek(dut.in.ready) === BigInt(1) && peek(dut.in.valid) === BigInt(1)) {
      write_counter = write_counter + 1
    }

    // Read output data
    if (peek(dut.out.ready) === BigInt(1) && peek(dut.out.valid) === BigInt(1)) {
      peekedValue = peek(dut.out.bits.data)
      if(verbose) {
        print(f"peeked: 0x${formatString(peekedValue, params.dataBytes)}, ")
        print(f"expected: 0x${formatString(inData(read_counter), params.dataBytes)}\n")
      }
      assert(
        peekedValue == inData(read_counter),
        f"$write_counter. read value: 0x${formatString(peekedValue, params.dataBytes)}, but should be: 0x${formatString(inData(read_counter), params.dataBytes)}"
      )
      read_counter = read_counter + 1
    }
    step(1)
  }

  // Check CRC
  if (en) {
    var peekedCRC: BigInt = 0
    var peekedError: BigInt = 0

    while (peek(dut.io.o_crc_valid) != 1) {
      step(1)
    }
    assert(
      peek(dut.io.o_crc_valid) == 1,
      f"o_crc_valid: ${peek(dut.io.o_crc_valid)}, but should be 1.\n"
    )
    peekedCRC = peek(dut.io.o_crc)
    peekedError = peek(dut.io.o_error)
    if (verbose) {
      print(f"peeked CRC: 0x$peekedCRC%08X, ")
      print(f"expected CRC: 0x$crcValue%08X\n")

      print(f"peeked CRC error: $peekedError, ")
      print(f"expected CRC error: 0.\n")
    }
    assert(
      peekedCRC == crcValue,
      f"Read CRC value: 0x$peekedCRC%08X, but should be: 0x$crcValue%08X\n"
    )
    assert(
      peekedError == 0,
      f"Read CRC error value: $peekedError, but should be: 0.\n"
    )
  }

  step(20)
  stepToCompletion(maxCycles = samplesExpected*8, silentFail = silentFail)
}
