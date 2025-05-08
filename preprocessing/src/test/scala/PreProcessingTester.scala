package preprocessing

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.diplomacy.lazymodule._

class PreProcessingTester
(
  dut: PreProcessingAXI4 with TestStandaloneAXI4Block,
  address: AddressSet,
  params : PreProcessingParameters,
  config: TestConfiguration,
  beatBytes : Int,
  silentFail: Boolean = false,
  verbose: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel[LazyModuleImp] with AXI4MasterModel with TestUtils {

  if (verbose) {
    print(
      f"\n###############################################################\n" +
      f"PreProcessingAXI4 with PreProcessingParameters:\n" +
      f"\tMaximum chipr sample size: ${params.MaxChirpSize},\n" +
      f"\tMaximum chiprs per frame : ${params.MaxChirpsPerFrame},\n" +
      f"\tCRC parameters:,\n" +
      f"\t\tData width    : ${params.CrcParams.dataBytes} bytes,\n" +
      f"\t\tCRC polynomial: ${params.CrcParams.polynomial}%08X,\n" +
      f"\t\tInitial value : ${params.CrcParams.init}%08X,\n" +
      f"\t\tXOR out value : ${params.CrcParams.xorOut}%08X,\n" +
      f"\t\tReflect in    : ${params.CrcParams.reflectIn},\n" +
      f"\t\tReflect out   : ${params.CrcParams.reflectOut},\n" +
      f"\tBuffer parameters:,\n" +
      f"\t\tBuffers enabled : ${params.BufferParams.insertBuffers},\n" +
      f"\t\tBuffer size     : ${params.BufferParams.size}.\n" +
      f"###############################################################\n"
    )
  }

  val mod: LazyModuleImp = dut.module
  // Bind nodes
  def memAXI: AXI4Bundle = dut.ioMem.get
  val inMaster: AXI4StreamPeekPokeMaster = bindMaster(dut.in.getWrappedValue)

  config.regs.foreach(regs => {
    // Check configuration and parameters
    assert(
      regs.chirpsize <= params.MaxChirpSize,
      f"Cannot set chirp size to ${regs.chirpsize} since max allowed chirp size is ${params.MaxChirpSize}."
    )
    assert(
      regs.expectedsize <= regs.chirpsize,
      f"Cannot set expected chirp size to ${regs.expectedsize} since set chirp size is ${regs.chirpsize}."
    )
    assert(
      regs.chirpperframe <= params.MaxChirpsPerFrame,
      f"Cannot set chirps per frame to ${regs.chirpperframe} since max allowed number is ${params.MaxChirpsPerFrame}."
    )

    // Reset stream nodes
    resetMaster(dut.in)
    resetSlave(dut.out)
    poke(dut.io.i_crc_data, false.B)
    step(1)
    // Write to memory
    memWriteWord(address.base + Regs.chirpsize,     regs.chirpsize)
    memWriteWord(address.base + Regs.expectedsize,  regs.expectedsize)
    memWriteWord(address.base + Regs.chirpperframe, regs.chirpperframe)
    memWriteWord(address.base + Regs.dataformat,    regs.dataformat)
    memWriteWord(address.base + Regs.ctrl,          regs.ctrl)

    poke(dut.out.ready, true.B)
    step(1)
    // Generate test array
    val inputBytes: Array[Byte] = Array.fill(
      regs.chirpperframe * params.CrcParams.dataBytes * (if (regs.dataformat == 0) regs.expectedsize else regs.expectedsize*2)
    )((scala.util.Random.nextInt(256) - 128).toByte)
    val inData: Seq[BigInt] = inputBytes.grouped(params.CrcParams.dataBytes).toSeq.map(
      m => m.reverse.foldLeft(BigInt(0)) { (acc, b) => (acc << 8) | (b & 0xFF) }
    )
    // Generate expected values for the output of the preprocessing block
    val expectedData = transform(
      inData,
      regs.ctrl,
      regs.dataformat,
      beatBytes,
      regs.chirpsize,
      regs.expectedsize,
      regs.chirpperframe
    )

    // Convert input data sequence to sequence of chirps
    val inByteSequence = inputBytes.grouped(params.CrcParams.dataBytes * (if (regs.dataformat == 0) regs.expectedsize else regs.expectedsize*2))

    // Send input data and then check the processed data
    inByteSequence.zipWithIndex.foreach { case (dataSeq, i) =>
      val inputData: Seq[BigInt] = dataSeq.grouped(params.CrcParams.dataBytes).toSeq.map(
        m => m.reverse.foldLeft(BigInt(0)) { (acc, b) => (acc << 8) | (b & 0xFF) }
      )
      // Calculate reference CRC value
      val crcValue: BigInt = BigInt(crc32(dataSeq, params.CrcParams, 32))
      // Send the CRC value to the input of the preprocessing block
      val inCRC: Seq[BigInt] = for (i <- 0 until 4 / params.CrcParams.dataBytes) yield {
        (crcValue >> (i * params.CrcParams.dataBytes * 8)) & ((BigInt(1) << (params.CrcParams.dataBytes * 8)) - 1)
      }
      // Add input data to AXI4Stream transactions
      inMaster.addTransactions(inputData.zipWithIndex.map {
        case (m, i) => AXI4StreamTransaction(data = m, last = i == inputData.length - 1)
      })
      // If CRC is enabled, send CRC data
      if ((regs.ctrl & 0x1) == 1) {
        inMaster.addTransactions(inCRC.map { m => AXI4StreamTransaction(data = m) })
      }

      var counter = 0
      var peekedValue: BigInt = 0
      val expectedChirpSize = expectedData.length / regs.chirpperframe
      while (counter < expectedChirpSize) {
        // Randomize ready and valid
        poke(dut.in.valid,  scala.util.Random.nextInt(2))
        poke(dut.out.ready, scala.util.Random.nextInt(2))
        // Check output data
        if (peek(dut.out.ready) === BigInt(1) && peek(dut.out.valid) === BigInt(1)) {
          peekedValue = peek(dut.out.bits.data)
          if (verbose) {
            print(f"peeked: 0x${formatString(peekedValue, beatBytes)}, ")
            print(f"expected: 0x${formatString(expectedData(i * expectedChirpSize + counter), beatBytes)}\n")
          }
          assert(
            peekedValue == expectedData(i * expectedChirpSize + counter),
            f"$counter. read value: 0x${formatString(peekedValue, beatBytes)}, but should be: 0x${formatString(expectedData(i * expectedChirpSize + counter), beatBytes)}"
          )
          counter = counter + 1
        }
        step(1)
      }
      step(1)
      // Check CRC result
      if ((regs.ctrl & 0x1) == 1) {
        val peekedCRC = memReadWord(address.base + Regs.crcvalue)
        val peekedCRCStatus = memReadWord(address.base + Regs.crcstatus)
        if (verbose) {
          print(f"peeked CRC: 0x$peekedCRC%08X, ")
          print(f"expected CRC: 0x$crcValue%08X\n")

          print(f"peeked CRC error: $peekedCRCStatus, ")
          print(f"expected CRC error: 0.\n")
        }
        assert(
          peekedCRC == crcValue,
          f"Read CRC value: 0x$peekedCRC%08X, but should be: 0x$crcValue%08X\n"
        )
        assert(
          peekedCRCStatus == BigInt(0),
          f"Read CRC status value: $peekedCRCStatus, but should be: 0.\n"
        )
      }
      step(20)
    }
    stepToCompletion(maxCycles = expectedData.length*8, silentFail = silentFail)
  })
}

