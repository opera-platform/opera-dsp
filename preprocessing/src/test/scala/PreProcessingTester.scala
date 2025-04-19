package preprocessing

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.diplomacy.lazymodule._
import treadle2.chronometry.Timer

// TODO: Solve problem with last valid of AXI4Stream master
// Current solution is to add buffer on the input side.
// Problem is that master generates last valid only half clock cycles long

class PreProcessingTester
(
  dut: PreProcessingAXI4 with PreProcessingAXI4Standalone,
  address: AddressSet,
  params : BlockParameters,
  config: TestConfiguration,
  beatBytes : Int,
  silentFail: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel[LazyModuleImp] with AXI4MasterModel with TestUtils {

  val timer = new Timer

  val mod = dut.module
  // Bind nodes
  def memAXI: AXI4Bundle = dut.ioMem.get
  val inMaster = bindMaster(dut.in.getWrappedValue)

  config.regs.foreach(regs => {
    // Check configuration and parameters
    assert(
      regs.chirpsize <= params.ChirpSize,
      f"Cannot set chirp size to ${regs.chirpsize} since max allowed chirp size is ${params.ChirpSize}."
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
    step(1)
    // Write to memory
    memWriteWord(address.base + Regs.chirpsize,     regs.chirpsize)
    memWriteWord(address.base + Regs.expectedsize,  regs.expectedsize)
    memWriteWord(address.base + Regs.chirpperframe, regs.chirpperframe)
    memWriteWord(address.base + Regs.dataformat,    regs.dataformat)
    memWriteWord(address.base + Regs.ctrl,          regs.ctrl)

    poke(dut.out.ready, true.B)
    step(1)
    // generate simple test array!
    val inData: Seq[BigInt] = Seq.tabulate(if (regs.dataformat == 0) regs.expectedsize else regs.expectedsize*2) { i => i }
    // Generate output of the model
    val expectedData = transform(
      inData,
      regs.ctrl,
      regs.dataformat,
      beatBytes,
      regs.chirpsize,
      regs.expectedsize,
      regs.chirpperframe
    )
    // Add transactions
    inMaster.addTransactions(inData.zipWithIndex.map {
      case (m, i) => AXI4StreamTransaction(data = m, last = i == inData.length - 1)
    })

    var counter = 0
    var peekedValue: BigInt = 0
    while (counter < expectedData.length) {
      if (peek(dut.out.ready) === BigInt(1) && peek(dut.out.valid) === BigInt(1)) {
        peekedValue = peek(dut.out.bits.data)
        print(f"peeked: 0x${formatString(peekedValue, beatBytes)}, ")
        print(f"expected: 0x${formatString(expectedData(counter), beatBytes)}\n")
        assert(
          peekedValue == expectedData(counter),
          f"$counter. read value: 0x${formatString(peekedValue, beatBytes)}, but should be: 0x${formatString(expectedData(counter), beatBytes)}"
        )
        counter = counter + 1
      }
      step(1)
    }

    step(20)
//    if (showTiming) {
//      println(s"\n${timer.report()}")
//    }
    stepToCompletion(maxCycles = 100, silentFail = silentFail)
  })
}

