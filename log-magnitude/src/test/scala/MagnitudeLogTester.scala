package opera.logmagnitude

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import fixedpoint._
import opera.common.SignalUtils
import opera.common.StringUtils.formatStringBinary

import scala.math.BigDecimal.double2bigDecimal
import scala.util.Random

class MagnitudeLogTester(
  dut        : MagnitudeLog[FixedPoint],
  params     : LogMagnitudeParams[FixedPoint],
  sampleSize : Int,
  verbose    : Boolean = true,
  random     : Boolean = true,
  dataRandom : Boolean = true
) extends PeekPokeTester(dut) with TestUtils with SignalUtils {

  // Input data width
  val inputWidth: Int = params.realType.get.getWidth
  // Output data width
  val outputWidth: Int = params.outputType.getWidth

  // Input binary points
  val inputBinPoint = params.realType match {
    case data: Some[FixedPoint]=> data.get.binaryPoint.get
    case _ => 0
  }

  // Log binary points
  val logBinPoint = params.logType match {
    case data: Some[FixedPoint] => data.get.binaryPoint.get
    case _ => 0
  }

  // Output binary points
  val outputBinPoint = params.outputType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }

  // generate test array
  val inData: Seq[BigInt] = Seq.tabulate(sampleSize) { i =>
    if (dataRandom)
      Random.nextLong((1 << inputWidth) - 1)
    else
      i * (1 << inputWidth) / sampleSize
  }

  // Calculate reference value
  val expectedData: Seq[Double] = inData.map { m =>
    logModel(m, inputBinPoint, logBinPoint, outputBinPoint, params.lutTableSize, params.trimType)
  }

  // Reset DeCoupled nodes
  step(1)
  poke(dut.io.in.valid , false.B)
  poke(dut.io.out.ready, false.B)
  step(1)

  // Assert out.ready
  poke(dut.io.out.ready, true.B)
  step(1)

  var read_counter  = 0
  var write_counter = 0
  var peekedValue: BigInt = 0
  var peekedLast: BigInt = false

  while (read_counter < sampleSize) {
    // Randomize ready
    poke(dut.io.out.ready, if (random) scala.util.Random.nextInt(2) else 1)
    poke(dut.io.in.valid, if (random) scala.util.Random.nextInt(2) else 1)
    // Write input data
    if (peek(dut.io.in.valid) == 1 && peek(dut.io.in.ready) == 1 && write_counter < inData.length) {
      poke(dut.io.in.bits.asSInt, inData(write_counter))
      if (write_counter == sampleSize - 1) poke(dut.io.i_last, true.B) else poke(dut.io.i_last, false.B)
      write_counter = write_counter + 1
    }
    // Check output data
    if (peek(dut.io.out.valid) == 1 && peek(dut.io.out.ready) == 1) {
      peekedValue = peek(dut.io.out.bits).head
      peekedLast = peek(dut.io.o_last)
      // Expected values
      val expected = (expectedData(read_counter) * scala.math.pow(2, outputBinPoint)).toBigInt
      val expectedLast = if (read_counter == sampleSize - 1) BigInt(1) else BigInt(0)

      // Print if enabled
      if (verbose) {
        val in = inData(read_counter)
        print(f"i: 0x$read_counter%04X, ")
        print(
          f"input binary: " +
          f"${formatStringBinary(in >> inputBinPoint, inputWidth - inputBinPoint)}." +
          f"${formatStringBinary(in & ((1 << inputBinPoint) - 1), inputBinPoint)} = "
        )
        print(f"${in.toDouble / scala.math.pow(2, inputBinPoint)}%18.15f, ")
        print(f"peeked data: ")
        print(f"${peekedValue.toDouble / scala.math.pow(2, outputBinPoint)}%18.15f, ")
        print(f"expected data: ${expected.toDouble / scala.math.pow(2, outputBinPoint)}%18.15f.\n")
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

    peek(dut.io.out.bits)
    step(1)
  }
  step(5)
}
