package opera.logmagnitude

import breeze.math.Complex
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.DspComplex
import fixedpoint._
import opera.common.StringUtils.formatStringBinary
import opera.common.{ArithmeticUtils, SignalUtils}

import scala.math.BigDecimal.double2bigDecimal
import scala.util.Random

class MagnitudeMuxedTester(
  dut        : MagnitudeMuxed[FixedPoint],
  params     : LogMagnitudeParams[FixedPoint],
  sampleSize : Int,
  select     : Int,
  verbose    : Boolean = true,
  random     : Boolean = true,
  dataRandom : Boolean = true
) extends PeekPokeTester(dut) with TestUtils with SignalUtils {

  // Data widths
  val inputWidth   : Int = params.inputType.getWidth / 2
  val outputWidth  : Int = params.outputType.getWidth
  val logInputWidth: Int = params.realType.get.getWidth
  val lutTableWidth: Int = params.lutTableWidth.get

  // Data binary points
  val inputBinPoint = params.inputType match {
    case data: DspComplex[FixedPoint] => data.real.binaryPoint.get
    case _ => 0
  }
  val logInputBinPoint = params.realType match {
    case data: Some[FixedPoint] => data.get.binaryPoint.get
    case _ => 0
  }
  val outputBinPoint = params.outputType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }

  // Generate test input
  val inData: Seq[Complex] = if (dataRandom) Seq.fill(sampleSize) {
    Complex(Random.nextLong((1 << inputWidth) - 1).toDouble, Random.nextLong((1 << inputWidth) - 1).toDouble)
  } else {
    generateSignal(numSamples = sampleSize, freqReal1 = sampleSize / 15.42, freqImag1 = sampleSize / 3.14).map(c =>
      Complex(c.real * scala.math.pow(2, inputBinPoint), c.imag * scala.math.pow(2, inputBinPoint))
    )
  }

  // Convert complex data to Seq[BigInt]
  val inDataComplex: Seq[(BigInt, BigInt)] = complexToAXI4StreamSequence(inData, inputWidth).map { data =>
    val real = ArithmeticUtils.toSignedNBits(data.toLong >> inputWidth, inputWidth)
    val imag = ArithmeticUtils.toSignedNBits(data.toLong & ((1 << inputWidth) - 1), inputWidth)
    (real, imag)
  }

  // Calculate reference value
  val expectedData: Seq[BigInt] = inDataComplex.map { case (real, imag) =>
    // JPL model
    val jpl = jplModel(
      real           = real.toLong,
      imag           = imag.toLong,
      inputBinPoint  = inputBinPoint,
      outputBinPoint = if (params.magType == LogJPLSquared) logInputBinPoint else outputBinPoint,
      trimType       = params.trimType
    )

    // Square model depends on the position of MagnitudeSquare in the Magnitude Chain
    val squared = squareModel(
        real           = real.toLong,
        imag           = imag.toLong,
        inputBinPoint  = inputBinPoint,
        outputBinPoint = if (params.magType == LogJPLSquared) outputBinPoint else logInputBinPoint,
        trimType       = params.trimType
      )
    // Log Model depends on the Magnitude type
    val logDouble = logModel(
      data           = if (params.magType == LogJPLSquared) jpl else squared,
      inputBinPoint  = inputBinPoint,
      lutTableWidth  = lutTableWidth,
      outputBinPoint = outputBinPoint,
      lutTableSize   = params.lutTableSize.get,
      trimType       = params.trimType
    )
    val log = (logDouble * scala.math.pow(2, outputBinPoint)).toBigInt
    // Expected output depends on select value and Magnitude type
    if (select == 1) {
      log
    } else {
      if (params.magType == LogJPLSquared) squared else jpl
    }
  }

  // Reset values
  step(1)
  poke(dut.io.i_sel.get, select)
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
    if (peek(dut.io.in.valid) == 1 && peek(dut.io.in.ready) == 1 && write_counter < inDataComplex.length) {
      val real = inDataComplex(write_counter)._1
      val imag = inDataComplex(write_counter)._2
      poke(dut.io.in.bits.real.asSInt, real)
      poke(dut.io.in.bits.imag.asSInt, imag)
      if (write_counter == sampleSize - 1) poke(dut.io.i_last, true.B) else poke(dut.io.i_last, false.B)
      write_counter = write_counter + 1
    }
    // Check output data
    if (peek(dut.io.out.valid) == 1 && peek(dut.io.out.ready) == 1) {
      peekedValue = peek(dut.io.out.bits).head
      peekedLast = peek(dut.io.o_last)
      // Expected values
      val expected = expectedData(read_counter)
      val expectedLast = if (read_counter == sampleSize - 1) BigInt(1) else BigInt(0)

      // Print if enabled
      if (verbose) {
        val in_real = inDataComplex(read_counter)._1
        val in_imag = inDataComplex(read_counter)._2
        print(f"i: 0x$read_counter%04X, ")
        print(
          f"input: " +
            f"${formatStringBinary(in_real >> inputBinPoint, inputWidth - inputBinPoint)}." +
            f"${formatStringBinary(in_real & ((1 << inputBinPoint) - 1), inputBinPoint)} + "
        )
        print(
          f"${formatStringBinary(in_imag >> inputBinPoint, inputWidth - inputBinPoint)}." +
          f"${formatStringBinary(in_imag & ((1 << inputBinPoint) - 1), inputBinPoint)}j = "
        )
        print(f"(${in_real.toDouble / scala.math.pow(2, inputBinPoint)}%18.15f) + " +
              f"(${in_imag.toDouble / scala.math.pow(2, inputBinPoint)}%18.15f)j, ")
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
    step(1)
  }
  step(20)
}
