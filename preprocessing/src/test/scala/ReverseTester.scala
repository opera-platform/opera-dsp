package opera.preprocessing

import chisel3._
import chisel3.util.{Decoupled, Reverse}
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4stream._
import opera.common.{StandaloneAXI4StreamBlock, TestAXI4StreamBlock}
import org.chipsalliance.diplomacy.lazymodule._

class ReverseTester
(
  dut       : Reverse with TestAXI4StreamBlock,
  en        : Boolean = true,
  dataSize  : Int,
  dataRandom: Boolean = false,
  beatBytes : Int,
  random    : Boolean,
  verbose   : Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamRandomMasterModel[LazyModuleImp] with TestUtils {

  if (verbose) {
    print(f"\n###################################\n")
    print(f"# Reverse options: \n")
    print(f"# \tenable     = $en\n")
    print(f"# \tdataSize   = $dataSize\n")
    print(f"# \tdataRandom = $dataRandom\n")
    print(f"# \tbeatBytes  = $beatBytes\n")
    print(f"###################################\n")
  }

  val mod: LazyModuleImp = dut.module
  // Bind nodes
  val inMaster: AXI4StreamRandomPeekPokeMaster = bindMaster(dut.in.getWrappedValue, random)

  // Reset stream nodes
  resetMaster(dut.in)
  poke(dut.out.ready, false.B)
  poke(dut.io.i_en, en)
  step(1)

  poke(dut.out.ready, true.B)
  step(1)
  // generate test array
  val inData: Seq[BigInt] = Seq.tabulate(dataSize) { i => if(dataRandom) scala.util.Random.between(0, 1 << beatBytes*8).toBigInt else i }
  // Generate output of the model
  val expectedData: Seq[BigInt] = reverse(inData, en, beatBytes)
  // Add transactions
  inMaster.addTransactions(inData.zipWithIndex.map {
    case (m, i) => AXI4StreamTransaction(data = m, last = i == inData.length - 1)
  })

  var counter = 0
  var peekedValue: BigInt = 0
  while (counter < expectedData.length) {
    // Randomize ready
    if (random) poke(dut.out.ready, scala.util.Random.nextInt(2))
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
}


// Module Wrapper for formal verification
class ReverseWrapper
(
  beatBytes: Int,
  en: Boolean = true
) extends Module {

  val lazyMod = LazyModule(new Reverse with StandaloneAXI4StreamBlock {
    override def dataBytes: Int = beatBytes
  })

  val mod = Module(lazyMod.module)

  val input = IO(Flipped(Decoupled(chiselTypeOf(lazyMod.in.bits))))
  val output = IO(Decoupled(chiselTypeOf(lazyMod.out.bits)))

  input <> lazyMod.in
  output <> lazyMod.out
  lazyMod.io.i_en := en.B

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
