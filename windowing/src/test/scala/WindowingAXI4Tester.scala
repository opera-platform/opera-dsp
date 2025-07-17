package opera.windowing

import breeze.math.Complex
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.Convergent
import fixedpoint._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy.AddressSet
import opera.common.{ArithmeticUtils, TestStandaloneAXI4Block, SignalUtils}
import org.chipsalliance.diplomacy.lazymodule.LazyModuleImp

import scala.math.BigDecimal.double2bigDecimal

class WindowingAXI4Tester(
  dut              : WindowingAXI4[FixedPoint] with TestStandaloneAXI4Block,
  csrAddress       : AddressSet,
  ramAddress       : AddressSet,
  windowFuncRunTime: WindowType,
  params           : WindowingParams[FixedPoint],
  freq             : Double = 15.54 / 1024,
  beatBytes        : Int = 4,
  enable           : Boolean = true,
  verbose          : Boolean = true,
  random           : Boolean = true
) extends PeekPokeTester(dut.module)
  with AXI4StreamRandomMasterModel[LazyModuleImp]
  with AXI4MasterModel
  with SignalUtils
  with TestUtils {

  if (verbose) {
    print(f"\n#####################################\n")
    print(f"# Windowing options: \n")
    print(f"# \twindow enabled = $enable\n")
    print(f"# \twindow size    = ${params.numPoints}\n")
    print(f"# \twindow type    = ${params.windowFunc.toString}\n")
    print(f"# \trun-time       = ${params.runTime}\n")
    print(f"# \tROM for coeffs = ${params.constWindow}\n")
    print(f"# \tbeatBytes      = $beatBytes\n")
    print(f"#####################################\n")
  }

  val mod: LazyModuleImp = dut.module

  // Input data width
  val dataWidth: Int = params.dataType.getWidth

  // Bind nodes
  def memAXI: AXI4Bundle = dut.ioMem.get
  val inMaster: AXI4StreamRandomPeekPokeMaster = bindMaster(dut.in.getWrappedValue, random = random)

  // Data binary points
  val dataBinPoint = params.dataType.real match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }

  // Window coefficient's binary points
  val winBinPoint = params.coeffType match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }

  // Expected chirps size
  val numPoints: Int = if (params.runTime & !params.constWindow) windowFuncRunTime.N else params.numPoints

  // Get window function
  val window: Option[Seq[Double]] =
    if (params.runTime & !params.constWindow) windowFuncRunTime.function else params.windowFunc.function

  // Generate test array
  val inData: Seq[Complex] = generateSignal(numSamples = numPoints, freqReal1 = freq, freqImag1 = freq).map(c =>
    Complex(c.real * scala.math.pow(2, dataBinPoint), c.imag * scala.math.pow(2, dataBinPoint))
  )

  // Convert complex data to adequate AXI4Stream format (i.e. Int)
  val inDataComplex: Seq[BigInt] = complexToAXI4StreamSequence(inData, dataWidth / 2)

  // Calculate reference value
  // If windowing function is defined and windowing is enabled, calculate the result
  // Otherwise, output of the block should be the same as input
  val expectedData: Seq[(BigInt, BigInt)] = if (window.isDefined & params.windowFunc.function.isDefined & enable)
    inDataComplex.zip(window.get).map {
      case (data, coefficient) =>
        windowModel(
          data        = data,
          coefficient = coefficient,
          dataWidth   = dataWidth,
          winBinPoint = winBinPoint,
          trimType    = params.trimType
        )
    }
  else inData.map {
    data =>
      val real = ArithmeticUtils.toSignedNBits(data.real.toInt, dataWidth / 2)
      val imag = ArithmeticUtils.toSignedNBits(data.imag.toInt, dataWidth / 2)
      (real, imag)
  }

  // If run-time is enabled and RAM is used to store coefficients, write function coefficients to memory
  if (params.runTime & window.isDefined & !params.constWindow & params.windowFunc.function.isDefined) {
    window.get.zipWithIndex.foreach{ case (m, i) =>
      val coefficient = ArithmeticUtils.roundWithMode(m * (1 << winBinPoint), Convergent).toBigInt
      memWriteWord(ramAddress.base + beatBytes * i, coefficient)
    }
    step(10)
  }

  // Reset stream nodes
  resetMaster(dut.in)
  poke(dut.out.ready, 0)
  step(1)

  // Write to memory
  val regs: Regs = Regs(beatBytes)
  memWriteWord(csrAddress.base + regs.ctrl, enable)
  memWriteWord(csrAddress.base + regs.chirpsize, params.numPoints)

  // Assert out.ready
  poke(dut.out.ready, true.B)
  step(1)

  // Add input data to AXI4Stream transactions
  inMaster.addTransactions(inDataComplex.indices.map(i => AXI4StreamTransaction(data = inDataComplex(i))))
  inMaster.addTransactions(inDataComplex.zipWithIndex.map {
    case (data, idx) => AXI4StreamTransaction(data = data, last = if (idx == inDataComplex.length - 1) true else false)
  })

  // We are checking only one data window
  var counter = 0
  var peekedValue: BigInt = 0
  while (counter < numPoints) {
    // Randomize ready
    poke(dut.out.ready, if (random) scala.util.Random.nextInt(2) else 1)
    // Check output data
    if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
      peekedValue = peek(dut.out.bits.data)
      val real = ArithmeticUtils.toSignedNBits(peekedValue >> (dataWidth / 2), dataWidth / 2)
      val imag = ArithmeticUtils.toSignedNBits(peekedValue & ((1 << (dataWidth/2)) - 1), dataWidth / 2)
      // Print if enabled
      if (verbose & window.isDefined) {
        val in_real = ArithmeticUtils.toSignedNBits(inDataComplex(counter) >> (dataWidth / 2), dataWidth / 2)
        val in_imag = ArithmeticUtils.toSignedNBits(inDataComplex(counter) & ((1 << (dataWidth/2)) - 1), dataWidth / 2)
        val coef = ArithmeticUtils.roundWithMode(window.get(counter) * (1 << winBinPoint), Convergent).toBigInt
        print(f"i: 0x$counter%04X, ")
        print(f"input: $in_real%6d + $in_imag%6dj, coefficient: $coef%6d, ")
        print(f"peeked data: $real%6d + $imag%6dj, ")
        print(f"expected data: ${expectedData(counter)._1}%6d + ${expectedData(counter)._2}%6dj.\n")
      }
      // Check results
      require(
        expectedData(counter)._1 == real & expectedData(counter)._2 == imag,
        f"[0x$counter%04X] Expected and received data are different.\n" +
          f"\texpected: ${expectedData(counter)._1} + j${expectedData(counter)._2},\n" +
          f"\treceived: $real + j$imag,\n"
      )
      counter = counter + 1
    }
    step(1)
  }
  step(20)
}
