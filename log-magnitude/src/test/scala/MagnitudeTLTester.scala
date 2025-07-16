package opera.logmagnitude

import breeze.math.Complex
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.DspComplex
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.{OptionalTLMasterModel, TLBundle}
import opera.common.StringUtils.formatStringBinary
import opera.common.{ArithmeticUtils, SignalUtils, StandaloneTLBlock}
import org.chipsalliance.diplomacy.lazymodule.LazyModuleImp

import scala.math.BigDecimal.double2bigDecimal
import scala.util.Random

class MagnitudeTLTester(
  dut        : MagnitudeTL[FixedPoint] with StandaloneTLBlock,
  params     : LogMagnitudeParams[FixedPoint],
  sampleSize : Int,
  verbose    : Boolean = true,
  random     : Boolean = true,
  address    : AddressSet,
  beatBytes  : Int
) extends PeekPokeTester(dut.module) with OptionalTLMasterModel with TestUtils with SignalUtils {

  val mod: LazyModuleImp = dut.module

  // Bind nodes
  def memTL: Option[TLBundle] = if (dut.ioMem.isDefined) Some(dut.ioMem.get) else None

  // Data widths
  val inputWidth   : Int = if (params.magType == Log) params.realType.get.getWidth else params.inputType.getWidth / 2
  val outputWidth  : Int = params.outputType.getWidth
  val lutTableWidth: Option[Int] = params.lutTableWidth

  // Data binary points
  val inputBinPoint = params.inputType match {
    case data: DspComplex[FixedPoint] => data.real.binaryPoint.get
    case _ => 0
  }
  val realBinPoint = params.realType match {
    case data: Some[FixedPoint] => data.get.binaryPoint.get
    case _ => 0
  }
  val outputBinPoint = params.outputType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }

  val selectSeq: Seq[Int] = if (params.magType == LogSquaredJPL || params.magType == LogJPLSquared) Seq(0, 1) else Seq(0)

  for (select <- selectSeq) {
    // Generate test input
    val inData: Seq[Complex] = Seq.fill(sampleSize) {
      Complex(Random.nextLong((1 << inputWidth) - 1).toDouble, Random.nextLong((1 << inputWidth) - 1).toDouble)
    }
    val inputLogData: Seq[BigInt] = Seq.fill(sampleSize) { Random.nextLong((1 << inputWidth) - 1) }

    // Convert data to adequate AXI4Stream format
    val inputStreamData: Seq[BigInt] =
      if (params.magType == Log) {
        inputLogData
      } else {
        complexToAXI4StreamSequence(inData, inputWidth)
      }

    val inDataComplex: Seq[(BigInt, BigInt)] = inputStreamData.map { data =>
      val real = ArithmeticUtils.toSignedNBits(data.toLong >> inputWidth, inputWidth)
      val imag = ArithmeticUtils.toSignedNBits(data.toLong & ((1 << inputWidth) - 1), inputWidth)
      (real, imag)
    }


    // Calculate reference value
    val expectedData: Seq[BigInt] = params.magType match {
      case JPL =>
        inDataComplex.map {case (real, imag) =>
          jplModel(
            real           = real.toLong,
            imag           = imag.toLong,
            inputBinPoint  = inputBinPoint,
            outputBinPoint = if (params.magType == LogJPLSquared) realBinPoint else outputBinPoint,
            trimType       = params.trimType
          )
        }

      case Squared =>
        inDataComplex.map { case (real, imag) =>
          squareModel(
            real           = real.toLong,
            imag           = imag.toLong,
            inputBinPoint  = inputBinPoint,
            outputBinPoint = outputBinPoint,
            trimType       = params.trimType
          )
        }

      case Log =>
        inputLogData.map { data =>
          val logDouble = logModel(
            data           = data,
            inputBinPoint  = realBinPoint,
            lutTableWidth  = lutTableWidth.get,
            outputBinPoint = outputBinPoint,
            lutTableSize   = params.lutTableSize.get,
            trimType       = params.trimType
          )
          (logDouble * scala.math.pow(2, outputBinPoint)).toBigInt
        }

      case LogSquaredJPL | LogJPLSquared =>
        inDataComplex.map { case (real, imag) =>
          // JPL model
          val jpl = jplModel(
            real           = real.toLong,
            imag           = imag.toLong,
            inputBinPoint  = inputBinPoint,
            outputBinPoint = if (params.magType == LogJPLSquared) realBinPoint else outputBinPoint,
            trimType       = params.trimType
          )
          // Square model depends on the position of MagnitudeSquare in the Magnitude Chain
          val squared = squareModel(
            real           = real.toLong,
            imag           = imag.toLong,
            inputBinPoint  = inputBinPoint,
            outputBinPoint = if (params.magType == LogSquaredJPL) realBinPoint else outputBinPoint,
            trimType       = params.trimType
          )
          // Log Model depends on the Magnitude type
          val logDouble = logModel(
            data = if (params.magType == LogJPLSquared) jpl else squared,
            inputBinPoint  = realBinPoint,
            lutTableWidth  = lutTableWidth.get,
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
    }

    // Reset values
    poke(dut.in.valid, 0)
    poke(dut.out.ready, 0)
    step(1)

    // Write to memory
    if (memTL.isDefined) memWriteWord(address.base, select, beatBytes)
    step(5)
    // Assert out.ready
    poke(dut.out.ready, true.B)
    step(1)

    var read_counter = 0
    var write_counter = 0
    var peekedValue: BigInt = 0
    var peekedLast: BigInt = false

    while (read_counter < sampleSize) {
      // Randomize ready
      poke(dut.out.ready, if (random) scala.util.Random.nextInt(2) else 1)
      poke(dut.in.valid, if (random) scala.util.Random.nextInt(2) else 1)
      // Write input data
      if (peek(dut.in.valid) == 1 && peek(dut.in.ready) == 1 && write_counter < inData.length) {
        poke(dut.in.bits.data, inputStreamData(write_counter))
        if (write_counter == sampleSize - 1) poke(dut.in.bits.last, true.B) else poke(dut.in.bits.last, false.B)
        write_counter = write_counter + 1
      }
      // Check output data
      if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
        peekedValue = ArithmeticUtils.toSignedNBits(peek(dut.out.bits.data), outputWidth)
        peekedLast = peek(dut.out.bits.last)
        // Expected values
        val expected = expectedData(read_counter)
        val expectedLast = if (read_counter == sampleSize - 1) BigInt(1) else BigInt(0)

        // Print if enabled
        if (verbose) {
          print(f"i: 0x$read_counter%04X, ")
          if (params.magType == Log) {
            val in = inputLogData(read_counter)
            print(
              f"input binary: " +
                f"${formatStringBinary(in >> inputBinPoint, inputWidth - inputBinPoint)}." +
                f"${formatStringBinary(in & ((1 << inputBinPoint) - 1), inputBinPoint)} = "
            )
          } else {
            val in_real = inDataComplex(read_counter)._1
            val in_imag = inDataComplex(read_counter)._2
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
          }
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
    step(5)
  }
}
