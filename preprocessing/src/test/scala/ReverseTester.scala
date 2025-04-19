package preprocessing

import chisel3._
import chisel3.util.{Decoupled, Reverse}
import chiseltest.experimental.observe
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.bundlebridge.{BundleBridgeSink, BundleBridgeSource}

import scala.util.Random

class ReverseTester
(
  dut: Reverse with TestAXI4StreamBlock,
  en: Boolean = true,
  dataSize: Int,
  dataRandom: Boolean = false,
  beatBytes: Int,
  silentFail: Boolean = false,
  verbose: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel[LazyModuleImp] with TestUtils {

  if (verbose) {
    print(f"\n###################################\n")
    print(f"# Reverse options: \n")
    print(f"# \tenable     = $en\n")
    print(f"# \tdataSize   = $dataSize\n")
    print(f"# \tdataRandom = $dataRandom\n")
    print(f"# \tbeatBytes  = $beatBytes\n")
    print(f"###################################\n")
  }

  val mod = dut.module
  // Bind nodes
  val inMaster = bindMaster(dut.in.getWrappedValue)

  // Reset stream nodes
  resetMaster(dut.in)
  resetSlave(dut.out)
  poke(dut.io.en_rev, en)
  step(1)

  poke(dut.out.ready, true.B)
  step(1)
  // generate test array
  val inData: Seq[BigInt] = Seq.tabulate(dataSize) { i => if(dataRandom) Random.between(0, 1 << beatBytes*8).toBigInt else i }
  // Generate output of the model
  val expectedData = reverse(inData, en, beatBytes)
  // Add transactions
  inMaster.addTransactions(inData.zipWithIndex.map {
    case (m, i) => AXI4StreamTransaction(data = m, last = i == inData.length - 1)
  })

  var counter = 0
  var peekedValue: BigInt = 0
  while (counter < expectedData.length) {
    poke(dut.out.ready, Random.between(0,2))
    if (peek(dut.out.ready) === BigInt(1) && peek(dut.out.valid) === BigInt(1)) {
      peekedValue = peek(dut.out.bits.data)
      if(verbose) {
        print(f"peeked: 0x${formatString(peekedValue, beatBytes)}, ")
        print(f"expected: 0x${formatString(expectedData(counter), beatBytes)}\n")
      }
      assert(
        peekedValue == expectedData(counter),
        f"$counter. read value: 0x${formatString(peekedValue, beatBytes)}, but should be: 0x${formatString(expectedData(counter), beatBytes)}"
      )
      counter = counter + 1
    }
    step(1)
  }

  step(20)
  stepToCompletion(maxCycles = 100, silentFail = silentFail)
}


// Module Wrapper for formal verification
class ReverseWrapper
(
  beatBytes: Int,
  en: Boolean = true
) extends Module {

  val lazyMod = LazyModule(new Reverse {
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

    ioOutNode :=
      AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
      streamNode :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) :=
      ioInNode

    val in = InModuleBody { ioInNode.makeIO() }
    val out = InModuleBody { ioOutNode.makeIO() }
  })
  val mod = Module(lazyMod.module)

  val input = IO(Flipped(Decoupled(chiselTypeOf(lazyMod.in.bits))))
  val output = IO(Decoupled(chiselTypeOf(lazyMod.out.bits)))

  input <> lazyMod.in
  output <> lazyMod.out
  lazyMod.io.en_rev := en.B

  var inData: Seq[UInt] = Seq[UInt]()
  when(input.fire) {
    inData = inData :+ input.bits.data
  }


  when(output.fire) {
    if(en) {
      assert(output.bits.data === Reverse(inData.head))
    } else {
      assert(output.bits.data === inData.head)
    }
    inData = inData.tail
  }
}
