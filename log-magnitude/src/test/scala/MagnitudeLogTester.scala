package opera.logmagnitude

import breeze.math.Complex
import breeze.numerics.log
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.DspComplex
import fixedpoint._
import opera.common.StringUtils.formatStringBinary
import opera.common.{ArithmeticUtils, SignalUtils}

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
      Random.nextLong(1 << (inputWidth - 1))
    else
      i * (1 << (inputWidth - 1)) / sampleSize + 1
  }

  // Calculate reference value
  val expectedData: Seq[Double] = inData.map { m =>
    val mScaled = m >> (inputBinPoint - params.lutTableSize)
    val mCropped = mScaled << (inputBinPoint - params.lutTableSize)
    val log2 =
      if (mCropped != 0)
        log(mCropped.toDouble / scala.math.pow(2, inputBinPoint)) / log(2)
      else
        -inputBinPoint
    val log2scaled = log2 * scala.math.pow(2, logBinPoint)
    val log2rounded = ArithmeticUtils.roundWithMode(log2scaled, params.trimType).toBigInt
    log2rounded.toDouble / scala.math.pow(2, logBinPoint)
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

  while (read_counter < sampleSize) {
    // Randomize ready
    poke(dut.io.out.ready, if (random) scala.util.Random.nextInt(2) else 1)
    poke(dut.io.in.valid, if (random) scala.util.Random.nextInt(2) else 1)
    // Write input data
    if (peek(dut.io.in.valid) == 1 && peek(dut.io.in.ready) == 1 && write_counter < inData.length) {
      poke(dut.io.in.bits.asSInt, inData(write_counter))
      write_counter = write_counter + 1
    }
    // Check output data
    if (peek(dut.io.out.valid) == 1 && peek(dut.io.out.ready) == 1) {
      peekedValue = peek(dut.io.out.bits).head
      // Expected value
      val expected = expectedData(read_counter)

      // Print if enabled
      if (verbose) {
        val in = inData(read_counter)
        print(f"i: 0x$read_counter%04X, ")
        print(
          f"input binary: " +
          f"${formatStringBinary(in >> inputBinPoint, inputWidth - inputBinPoint)}." +
          f"${formatStringBinary(in & ((1 << inputBinPoint) - 1), inputBinPoint)}, "
        )
        print(f"input float: ${in.toDouble / scala.math.pow(2, inputBinPoint) }% 3.15f, ")
        print(f"peeked data: ${1.0 * peekedValue.toLong / scala.math.pow(2, outputBinPoint)}% 3.15f, ")
        print(f"expected data: $expected% 3.15f.\n")
      }
      // Check results
      require(
        (expected * scala.math.pow(2, outputBinPoint)).toBigInt == peekedValue,
        f"[0x$read_counter%04X] Expected and received data are different.\n" +
          f"\texpected: ${(expected * scala.math.pow(2, outputBinPoint)).toBigInt}, " +
          f"\treceived: $peekedValue\n"
      )
      read_counter = read_counter + 1
    }

    peek(dut.io.out.bits)
    step(1)
  }

  step(20)
}
