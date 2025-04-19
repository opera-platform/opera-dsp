package preprocessing

import chisel3._
import chisel3.util.{Cat, Decoupled}
import chiseltest.formal.past
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.bundlebridge.{BundleBridgeSink, BundleBridgeSource}

import scala.util.Random

class SwapTester
(
  dut: Swap with TestAXI4StreamBlock,
  en: Boolean = true,
  format: Int,
  dataSize: Int,
  dataRandom: Boolean = false,
  beatBytes: Int,
  silentFail: Boolean = false,
  verbose: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel[LazyModuleImp] with TestUtils {

  if (verbose) {
    print(f"\n###################################\n")
    print(f"# Swap options: \n")
    print(f"# \tenable     = $en\n")
    print(f"# \tformat     = $format\n")
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
  poke(dut.io.en_swap, en)
  poke(dut.io.r_format, format.U)
  step(1)

  poke(dut.out.ready, true.B)
  step(1)
  // generate test array
  val inData: Seq[BigInt] = Seq.tabulate(dataSize) { i => if(dataRandom) Random.between(0, 1 << beatBytes*8/2).toBigInt else i }
  // Generate output of the model
  val expectedData = swap(inData, format, en, beatBytes/2)
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
class SwapWrapper
(
  beatBytes: Int,
  en: Boolean = true,
  format: Int
) extends Module {

  val lazyMod = LazyModule(new Swap(beatBytes) {
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes/2)))
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

    ioOutNode :=
      AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
      streamNode :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes/2)) :=
      ioInNode

    val in = InModuleBody { ioInNode.makeIO() }
    val out = InModuleBody { ioOutNode.makeIO() }
  })
  val mod = Module(lazyMod.module)

  val input = IO(Flipped(Decoupled(chiselTypeOf(lazyMod.in.bits))))
  val output = IO(Decoupled(chiselTypeOf(lazyMod.out.bits)))

  input <> lazyMod.in
  output <> lazyMod.out
  lazyMod.io.en_swap := en.B
  lazyMod.io.r_format := format.U

  var inData: Seq[UInt] = Seq[UInt]()
  when(input.fire) {
    inData = inData :+ input.bits.data
  }

  when(output.fire && mod.reset === false.B) {
    format match {
      case 0 => assert(output.bits.data === Cat(0.U((beatBytes*4).W), input.bits.data))
      case 1 =>
        if(inData.length == 2) {
          if (en) {
            assert(output.bits.data === Cat(inData(1), inData.head))
          }
          else {
            assert(output.bits.data === Cat(inData.head, inData(1)))
          }
          inData = inData.tail
        }
      case _ =>
        if(inData.length == 2) {
          assert(output.bits.data === Cat(inData.head, inData(1)))
          inData = inData.tail
        }
    }
    inData = inData.tail
  }
}
