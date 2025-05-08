package preprocessing

import chisel3._
import chisel3.util.{Decoupled, log2Ceil}
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.implicits._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.diplomacy.lazymodule._

import scala.util.Random

class PadderTester
(
  dut: Padder with TestAXI4StreamBlock,
  en: Boolean = true,
  samples: Int,
  samplesExpected: Int,
  chirps: Int,
  dataRandom: Boolean = false,
  beatBytes: Int,
  silentFail: Boolean = false,
  verbose: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel[LazyModuleImp] with TestUtils {

  if (verbose) {
    print(f"\n#####################################\n")
    print(f"# Padder options: \n")
    print(f"# \tenable          = $en\n")
    print(f"# \tsamples         = $samples\n")
    print(f"# \tsamplesExpected = $samplesExpected\n")
    print(f"# \tchirps          = $chirps\n")
    print(f"# \tdataRandom      = $dataRandom\n")
    print(f"# \tbeatBytes       = $beatBytes\n")
    print(f"#####################################\n")
  }

  val mod: LazyModuleImp = dut.module
  // Bind nodes
  val inMaster: AXI4StreamPeekPokeMaster = bindMaster(dut.in.getWrappedValue)

  // Reset stream nodes
  resetMaster(dut.in)
  resetSlave(dut.out)
  poke(dut.io.i_en, en)
  poke(dut.io.i_samples, samples.U)
  poke(dut.io.i_samples_expected, samplesExpected.U)
  poke(dut.io.i_chirps, chirps.U)
  step(1)

  poke(dut.out.ready, true.B)
  step(1)
  // generate test array
  val inData: Seq[BigInt] = Seq.tabulate(samplesExpected*chirps) { i => if(dataRandom) BigInt(beatBytes*8, Random) else i }
  // Generate output of the model
  val expectedData: Seq[BigInt] = pad(inData, en, samples, samplesExpected, chirps)
  // Add transactions
  inMaster.addTransactions(inData.zipWithIndex.map {
    case (m, i) => AXI4StreamTransaction(data = m, last = i == inData.length/chirps - 1)
  })

  var counter = 0
  var peekedValue: BigInt = 0
  while (counter < expectedData.length) {
    // Randomize ready and valid
    poke(dut.in.valid,  scala.util.Random.nextInt(2))
    poke(dut.out.ready, scala.util.Random.nextInt(2))
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
  stepToCompletion(maxCycles = expectedData.length*8, silentFail = silentFail)
}

// Module Wrapper for formal verification
class PadderWrapper
(
  en: Boolean = true,
  maxSamplesPerChirp: Int,
  maxChirpsPerFrame: Int,
  samples: Int,
  samplesExpected: Int,
  chirps: Int,
  beatBytes: Int,
  verbose: Boolean = false
) extends Module {

  val lazyMod = LazyModule(new Padder(maxSamplesPerChirp, maxChirpsPerFrame) with StandaloneAXI4StreamBlock {
    override def dataBytes: Int = beatBytes
  })

  val mod = Module(lazyMod.module)

  val input = IO(Flipped(Decoupled(chiselTypeOf(lazyMod.in.bits))))
  val output = IO(Decoupled(chiselTypeOf(lazyMod.out.bits)))

  input <> lazyMod.in
  output <> lazyMod.out
  lazyMod.io.i_en := en.B
  lazyMod.io.i_samples := samples.U
  lazyMod.io.i_samples_expected := samplesExpected.U
  lazyMod.io.i_chirps := chirps.U

  val lastFlag = RegInit(false.B)
  val counter = RegInit(0.U(log2Ceil(samples).W))

  // Keep track of input data
  var inData: Seq[UInt] = Seq[UInt]()
  var inLast: Seq[UInt] = Seq[UInt]()

  when(input.fire) {
    inData = inData :+ input.bits.data
    inLast = inLast :+ input.bits.last
    when(input.bits.last) { lastFlag := true.B }
  }

  if (verbose) {
    printf(p"counter: $counter, ")
    printf(p"input: ${input.bits.data}, ")
    printf(p"output: ${output.bits.data}\n")
  }

  // Check output
  when(output.fire && mod.reset === false.B) {
    when(!en.B) {
      assert(output.bits.data === inData.head)
    }.otherwise{
      when(counter <= (samplesExpected - 1).U && !lastFlag) {
        assert(output.bits.data === inData.head)
        inData = inData.tail
      }.elsewhen(counter > (samplesExpected - 1).U && counter <= (samples - 1).U || lastFlag) {
        assert(output.bits.data === 0.U)
      }
    }

    // Keep track of read data
    when(counter < (samples - 1).U) { counter := counter + 1.U}
    .otherwise {
      counter := 0.U
      lastFlag := false.B
    }
  }
}
