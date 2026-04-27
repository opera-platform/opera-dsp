package opera.fft

import chisel3._
import chisel3.util.log2Up
import chiseltest.iotesters.PeekPokeTester
import fixedpoint._
import opera.common.SignalUtils

import scala.util.Random

class BitReverseTester(
  dut: BitReverse[FixedPoint],
  params: BitReverseParams[FixedPoint],
  sampleSize: Int,
  verbose: Boolean = true,
  random: Boolean = true,
  
) extends PeekPokeTester(dut) with SignalUtils {

  // move this to trait and then use extends
  /**
    * Returns bit reversed index
    */
  def bit_reverse(in: Int, width: Int): Int = {
    import scala.math.pow
    var test = in
    var out = 0
    for (i <- 0 until width) {
      if (test / pow(2, width - i - 1) >= 1) {
        out += pow(2, i).toInt
        test -= pow(2, width - i - 1).toInt
      }
    }
    out
  }

  /**
    * Reordering data
    */
  def bitrevorder_data(testSignal: Seq[BigInt]): Seq[BigInt] = {
    val seqLength = testSignal.size
    val new_indices = (0 until seqLength).map(x => bit_reverse(x, log2Up(seqLength)))
    new_indices.map(x => testSignal(x))
  }


  // Data widths
  val inputWidth: Int = params.dataType.getWidth
  // generate test array
  val inData1: Seq[BigInt] = Seq.tabulate(sampleSize) { i => i }
  val inData2: Seq[BigInt] = Seq.tabulate(sampleSize) { i => i + sampleSize}
  val inData3: Seq[BigInt] = Seq.tabulate(sampleSize) { i => i + 1 }
  val inData4: Seq[BigInt] = Seq.tabulate(sampleSize) { i => i + sampleSize + 1}

  val input = inData1 ++ inData2 ++ inData3 ++ inData4//if (dut.params.bitReverseDir) bitrevorder_data(inData) else inData
  val output = bitrevorder_data(inData1) ++
               bitrevorder_data(inData2) ++
               bitrevorder_data(inData3) ++
               bitrevorder_data(inData4)

  // Reset DeCoupled nodes
  step(1)
  poke(dut.io.in.valid, false.B)
  poke(dut.io.out.ready, false.B)
  step(1)

  // Assert out.ready
  poke(dut.io.out.ready, true.B)
  step(1)

  var read_counter = 0
  var write_counter = 0
  var peekedValue: BigInt = 0
  var peekedLast: BigInt = false

  // TODO: peek real / imag
  while (read_counter < 4*sampleSize && write_counter < 8*sampleSize) {
    // Randomize ready
    if(peek(dut.io.in.valid) == 1) poke(dut.io.out.ready, if (random) scala.util.Random.nextInt(2) else 1)
    poke(dut.io.in.valid, if (random) scala.util.Random.nextInt(2) else 1)
    // Write input data
    if (peek(dut.io.in.valid) == 1 && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.bits.real.asSInt, input(write_counter % (4*sampleSize)))
      poke(dut.io.in.bits.imag.asSInt, input(write_counter % (4*sampleSize)))
      if (write_counter == sampleSize - 1) poke(dut.io.i_last, true.B) else poke(dut.io.i_last, false.B)
      write_counter = write_counter + 1
    }
    // Check output data
    if (peek(dut.io.out.valid) == 1 && peek(dut.io.out.ready) == 1) {
      peekedValue = peek(dut.io.out.bits.real).head
      peekedLast = peek(dut.io.o_last)
      // Expected values
      val expected = output(read_counter % (4*sampleSize))
      val expectedLast = if (read_counter % sampleSize == sampleSize - 1) BigInt(1) else BigInt(0)

      // Print if enabled
      if (verbose) {
        val in = input(read_counter % (4*sampleSize))
        print(f"i: $read_counter%02d, ")
        print(f"input data: $in%3d, ")
        print(f"peeked data: ")
        print(f"$peekedValue%3d, ")
        print(f"expected data: $expected%3d.\n")
      }
      // Check results
      require(
        expected == peekedValue,
        f"[0x$read_counter%04X] Expected and received data are different.\n" +
          f"\texpected: $expected, " +
          f"\treceived: $peekedValue\n"
      )
      require(
        expectedLast == peekedLast,
        f"[0x$read_counter%04X] Expected and received last signals are different.\n" +
          f"\texpected: $expectedLast, " +
          f"\treceived: $peekedLast\n"
      )
      read_counter = read_counter + 1
    }
    step(1)
  }
  step(5)
}

