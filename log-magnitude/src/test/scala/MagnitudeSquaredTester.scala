package opera.logmagnitude

import breeze.math.Complex
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.DspComplex
import fixedpoint._
import opera.common.{ArithmeticUtils, SignalUtils}

import scala.math.BigDecimal.double2bigDecimal

class MagnitudeSquaredTester(
  dut        : MagnitudeSquared[FixedPoint],
  params     : LogMagnitudeParams[FixedPoint],
  sampleSize : Int,
  verbose    : Boolean = true,
  random     : Boolean = true
) extends PeekPokeTester(dut) with TestUtils with SignalUtils {

  // Input data width
  val inputWidth: Int = params.inputType.getWidth
  // Output data width
  val outputWidth: Int = params.outputType.getWidth

  // Input binary points
  val inputBinPoint = params.inputType match {
    case data: DspComplex[FixedPoint] => data.real.binaryPoint.get
    case _ => 0
  }

  // Output binary points
  val outputBinPoint = params.outputType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }

  // Generate test array
  val inData: Seq[Complex] = generateSignal(numSamples = sampleSize, freqReal1 = sampleSize/15.42, freqImag1 = sampleSize/3.14).map(c =>
    Complex(c.real * scala.math.pow(2, inputBinPoint), c.imag * scala.math.pow(2, inputBinPoint))
  )

  // Convert complex data to Seq[BigInt]
  val inDataComplex: Seq[(BigInt, BigInt)] = complexToAXI4StreamSequence(inData, inputWidth / 2).map { data =>
    val real = ArithmeticUtils.toSignedNBits(data >> (inputWidth / 2), inputWidth / 2)
    val imag = ArithmeticUtils.toSignedNBits(data & ((1 << (inputWidth / 2)) - 1), inputWidth / 2)
    (real, imag)
  }

  // Calculate reference value
  // If windowing function is defined and windowing is enabled, calculate the result
  // Otherwise, output of the block should be the same as input
  val expectedData: Seq[BigInt] = inDataComplex.map {case (real, imag) =>
    val tmp = square(real.toLong, imag.toLong)
    val scaled = tmp.toDouble / scala.math.pow(2, 2 * inputBinPoint - outputBinPoint)
    val out = ArithmeticUtils.roundWithMode(scaled, params.trimType).toBigInt
    out
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
    if (peek(dut.io.in.valid) == 1 && peek(dut.io.in.ready) == 1 && write_counter < inDataComplex.length) {
      val real = inDataComplex(write_counter)._1
      val imag = inDataComplex(write_counter)._2
      poke(dut.io.in.bits.real.asSInt, real)
      poke(dut.io.in.bits.imag.asSInt, imag)
      write_counter = write_counter + 1
    }
    // Check output data
    if (peek(dut.io.out.valid) == 1 && peek(dut.io.out.ready) == 1) {
      peekedValue = peek(dut.io.out.bits).head
      // Expected value
      val expected = expectedData(read_counter)

      // Print if enabled
      if (verbose) {
        val in_real = inDataComplex(read_counter)._1
        val in_imag = inDataComplex(read_counter)._2
        print(f"i: 0x$read_counter%04X, ")
        print(f"input: $in_real%6d + $in_imag%6dj, ")
        print(f"peeked data: $peekedValue%6d, ")
        print(f"expected data: $expected%6d.\n")
      }
      // Check results
      require(
        expected == peekedValue,
        f"[0x$read_counter%04X] Expected and received data are different.\n" +
          f"\texpected: $expected, " +
          f"\treceived: $peekedValue\n"
      )
      read_counter = read_counter + 1
    }

    peek(dut.io.out.bits)
    step(1)
  }

  step(20)
}
