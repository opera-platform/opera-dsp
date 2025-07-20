package opera.windowing

import breeze.math.Complex
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.AddressSet
import opera.common.{ArithmeticUtils, SignalUtils, StandaloneAXI4Block}
import org.chipsalliance.diplomacy.lazymodule.LazyModuleImp

import scala.math.BigDecimal.double2bigDecimal
import scala.util.Random

class WindowingAXI4Tester(
  dut              : WindowingAXI4[FixedPoint] with StandaloneAXI4Block,
  csrAddress       : AddressSet,
  ramAddress       : AddressSet,
  windowFuncRunTime: WindowType,
  params           : WindowingParams[FixedPoint],
  beatBytes        : Int = 4,
  enable           : Boolean = true,
  verbose          : Boolean = true,
  random           : Boolean = true
) extends PeekPokeTester(dut.module)
  with AXI4MasterModel
  with SignalUtils
  with TestUtils {

  val mod: LazyModuleImp = dut.module
  // Bind nodes
  def memAXI: AXI4Bundle = dut.ioMem.get

  // Data widths
  val inputWidth : Int = params.inputType.getWidth / 2
  val outputWidth: Int = params.outputType.getWidth / 2
  val coeffWidth : Int = params.coeffType.getWidth
  // Data binary points
  val inputBinPoint = params.inputType match {
    case data: DspComplex[FixedPoint] => data.real.binaryPoint.get
    case _ => 0
  }
  val outputBinPoint = params.outputType match {
    case data: DspComplex[FixedPoint] => data.real.binaryPoint.get
    case _ => 0
  }
  val coeffBinPoint = params.coeffType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }

  // Expected chirps size
  val numPoints: Int = if (params.runTime & !params.constWindow) windowFuncRunTime.N else params.numPoints

  // Get window function
  val window: Option[Seq[Double]] =
    if (params.runTime & !params.constWindow & params.windowFunc.function.isDefined) {
      windowFuncRunTime.function
    } else {
      params.windowFunc.function
    }

  // Generate test array
  val inData: Seq[Complex] = Seq.fill(numPoints) {
    Complex(
      Random.nextLong((1 << inputWidth) - 1).toDouble,
      Random.nextLong((1 << inputWidth) - 1).toDouble
    )
  }
  // Convert data to adequate AXI4Stream format
  val inputStreamData: Seq[BigInt] = complexToAXI4StreamSequence(inData, inputWidth)
  // Format input data for widnowing model
  val inDataComplex: Seq[(BigInt, BigInt)] = inputStreamData.map { data =>
    val real = ArithmeticUtils.toSignedNBits(data.toLong >> inputWidth, inputWidth)
    val imag = ArithmeticUtils.toSignedNBits(data.toLong & ((1 << inputWidth) - 1), inputWidth)
    (real, imag)
  }

  // Calculate reference value
  // If windowing function is defined and windowing is enabled, calculate the result
  // Otherwise, output of the block should be the same as input
  val expectedData: Seq[(BigInt, BigInt)] = if (window.isDefined & params.windowFunc.function.isDefined & enable)
    inputStreamData.zip(window.get).map {
      case (data, coefficient) =>
        windowModel(
          inputData      = data,
          coefficient    = coefficient,
          inputWidth     = inputWidth,
          inputBinPoint  = inputBinPoint,
          outputBinPoint = outputBinPoint,
          coeffBinPoint  = coeffBinPoint,
          trimType       = params.trimType
        )
    }
  else inData.map {
    data =>
      val real = ArithmeticUtils.toSignedNBits(data.real.toInt, inputWidth)
      val imag = ArithmeticUtils.toSignedNBits(data.imag.toInt, inputWidth)
      (real, imag)
  }

  // If run-time is enabled and RAM is used to store coefficients, write function coefficients to memory
  if (params.runTime & window.isDefined & !params.constWindow & params.windowFunc.function.isDefined) {
    window.get.zipWithIndex.foreach{ case (m, i) =>
      val coefficient = ArithmeticUtils.roundWithMode(m * (1 << coeffBinPoint), Convergent).toBigInt
      memWriteWord(ramAddress.base + beatBytes * i, coefficient)
    }
    step(5)
  }

  // Reset values
  poke(dut.in.valid, 0)
  poke(dut.out.ready, 0)
  step(1)

  // Write to memory
  val regs: Regs = Regs(beatBytes)
  memWriteWord(csrAddress.base + regs.ctrl, enable)
  memWriteWord(csrAddress.base + regs.chirpsize, params.numPoints)

  // Assert out.ready
  poke(dut.out.ready, true.B)
  step(1)

  var read_counter = 0
  var write_counter = 0
  var peekedValue: BigInt = 0
  var peekedLast: BigInt = false

  while (read_counter < numPoints) {
    // Randomize ready
    poke(dut.out.ready, if (random) scala.util.Random.nextInt(2) else 1)
    poke(dut.in.valid, if (random) scala.util.Random.nextInt(2) else 1)
    // Write input data
    if (peek(dut.in.valid) == 1 && peek(dut.in.ready) == 1 && write_counter < inData.length) {
      poke(dut.in.bits.data, inputStreamData(write_counter))
      if (write_counter == numPoints - 1) poke(dut.in.bits.last, true.B) else poke(dut.in.bits.last, false.B)
      write_counter = write_counter + 1
    }
    // Check output data
    if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
      peekedValue = peek(dut.out.bits.data)
      peekedLast = peek(dut.out.bits.last)
      // Expected values
      val expected = expectedData(read_counter)
      val expectedLast = if (read_counter == numPoints - 1) BigInt(1) else BigInt(0)
      val real = ArithmeticUtils.toSignedNBits(peekedValue >> outputWidth, outputWidth)
      val imag = ArithmeticUtils.toSignedNBits(peekedValue & ((1 << outputWidth) - 1), outputWidth)
      // Print if enabled
      if (verbose) {
        val in_real = inDataComplex(read_counter)._1
        val in_imag = inDataComplex(read_counter)._2
        print(f"i: 0x$read_counter%04X, ")
        print(f"input: $in_real%6d + $in_imag%6dj,")
        if (window.isDefined) {
          val coef = ArithmeticUtils.roundWithMode(window.get(read_counter) * (1 << coeffBinPoint), Convergent).toBigInt
          print(f"coefficient: $coef%6d, ")
        }
        print(f"peeked data: $real%6d + $imag%6dj, ")
        print(f"expected data: ${expected._1}%6d + ${expected._2}%6dj.\n")
      }
      // Check results
      require(
        expected._1 == real && expected._2 == imag,
        f"[0x$read_counter%04X] Expected and received data are different.\n" +
          f"\texpected: ${expected._1} + ${expected._2}, " +
          f"\treceived: $real + ${imag}j\n"
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
