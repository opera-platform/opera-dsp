package opera.windowing

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.TrimType
import dsptools.numbers.{Ceiling, Convergent, Floor, Round}
import fixedpoint.FixedPoint
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4BundleParameters, AXI4MasterModel}
import freechips.rocketchip.amba.axi4stream.AXI4StreamBundle
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.{TLBundle, TLBundleParameters, TLMasterModel}
import opera.common.{ArithmeticUtils, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.double2bigDecimal
import scala.util.Random

object WindowingTestDut {
  def tl(
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      params: WindowingParams[FixedPoint],
      beatBytes: Int = 4,
      streamBytes: Int = 4
  )(implicit p: Parameters): WindowingTL[FixedPoint] with StandaloneTLBlock = LazyModule(
    new WindowingTL[FixedPoint](csrAddress, ramAddress, params, beatBytes)
      with StandaloneTLBlock {
        override def standaloneParams: TLBundleParameters = TLBundleParameters(
          addressBits = beatBytes * 8,
          dataBits = beatBytes * 8,
          sourceBits = 4,
          sinkBits = 1,
          sizeBits = 2,
          echoFields = Nil,
          requestFields = Nil,
          responseFields = Nil,
          hasBCE = false)

        override def dataBytes: Int = streamBytes
      })

  def axi4(
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      params: WindowingParams[FixedPoint],
      beatBytes: Int = 4,
      streamBytes: Int = 4
  )(implicit p: Parameters): WindowingAXI4[FixedPoint] with StandaloneAXI4Block = LazyModule(
    new WindowingAXI4[FixedPoint](csrAddress, ramAddress, Nil, params, beatBytes)
      with StandaloneAXI4Block {
        override def standaloneParams: AXI4BundleParameters = AXI4BundleParameters(
          addrBits = beatBytes * 8,
          dataBits = beatBytes * 8,
          idBits = 1)

        override def dataBytes: Int = streamBytes
      })
}

trait WindowingTestDriver { this: PeekPokeTester[_] =>
  def windowModel(
      inputData: BigInt,
      coefficient: Double,
      inputWidth: Int,
      inputBinPoint: Int,
      outputBinPoint: Int,
      coeffWidth: Int,
      coeffBinPoint: Int,
      trimType: TrimType
  ): (BigInt, BigInt) = {
    require(inputWidth + coeffWidth <= 52,
      "Double-based Windowing model would lose integer precision")
    trimType match {
      case Floor | Ceiling | Convergent | Round => ()
      case _ => throw new IllegalArgumentException(s"Unsupported Windowing trim type: $trimType")
    }
    val real = ArithmeticUtils.toSignedNBits(inputData >> inputWidth, inputWidth)
    val imag = ArithmeticUtils.toSignedNBits(inputData & ((1 << inputWidth) - 1), inputWidth)
    val coef = WindowCoefficientQuantizer.quantize(coefficient, coeffWidth, coeffBinPoint)
    // Scale date if necessary
    val scaledReal =
      if (inputBinPoint + coeffBinPoint > outputBinPoint) {
        real.toDouble * coef.toDouble /
          scala.math.pow(2, inputBinPoint + coeffBinPoint - outputBinPoint)
      } else {
        real.toDouble * coef.toDouble
      }
    val scaledImag =
      if (inputBinPoint + coeffBinPoint > outputBinPoint) {
        imag.toDouble * coef.toDouble /
          scala.math.pow(2, inputBinPoint + coeffBinPoint - outputBinPoint)
      } else {
        imag.toDouble * coef.toDouble
      }
    // Round the inputData
    val outReal = ArithmeticUtils.roundWithMode(scaledReal, trimType).toBigInt
    val outImag = ArithmeticUtils.roundWithMode(scaledImag, trimType).toBigInt
    (outReal, outImag)
  }

  def deterministicComplexData(count: Int, width: Int, seed: Long): Seq[BigInt] = {
    val limit = BigInt(1) << width
    val extrema = Seq(
      (BigInt(0), BigInt(0)),
      (BigInt(1), BigInt(-1)),
      (BigInt(-1), BigInt(1)),
      ((limit >> 1) - 1, -(limit >> 1)),
      (limit >> 2, -(limit >> 2)),
      (BigInt(3), BigInt(-3))
    )
    val rng = new Random(seed)
    val values = extrema ++ Seq.fill(math.max(0, count - extrema.length)) {
      (ArithmeticUtils.toSignedNBits(BigInt(width, rng), width),
        ArithmeticUtils.toSignedNBits(BigInt(width, rng), width))
    }
    val mask = limit - 1
    values.take(count).map { case (real, imag) => ((real & mask) << width) | (imag & mask) }
  }

  def runWindowingTest(
      input: AXI4StreamBundle,
      output: AXI4StreamBundle,
      writeWord: (BigInt, BigInt) => Unit,
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      windowFuncRunTime: WindowType,
      params: WindowingParams[FixedPoint],
      beatBytes: Int,
      enable: Boolean,
      verbose: Boolean,
      random: Boolean,
      numFrames: Int,
      shortFirstFrame: Boolean,
      checkDisabled: Boolean,
      seed: Long): Unit = {
    val inputWidth = params.inputType.getWidth / 2
    val outputWidth = params.outputType.getWidth / 2
    val coeffWidth = params.coeffType.getWidth
    val inputBinPoint = params.inputType.real.binaryPoint.get
    val outputBinPoint = params.outputType.real.binaryPoint.get
    val coeffBinPoint = params.coeffType.binaryPoint.get
    val runtimeRam = params.runTime && !params.constWindow
    val frameSize = if (runtimeRam) {
      windowFuncRunTime.N
    } else {
      params.numPoints
    }
    val window = if (runtimeRam) {
      windowFuncRunTime.function
    } else {
      params.windowFunc.function
    }
    val enabledFrameLengths = if (shortFirstFrame) {
      Seq(math.max(1, frameSize / 2), frameSize)
    } else {
      Seq.fill(numFrames)(frameSize)
    }
    val enabledSamples = enabledFrameLengths.sum
    val frameLengths = enabledFrameLengths ++ (if (checkDisabled) Seq(frameSize) else Nil)
    val totalSamples = frameLengths.sum
    val lastIndices = frameLengths.scanLeft(0)(_ + _).tail.map(_ - 1).toSet
    val inputData = deterministicComplexData(totalSamples, inputWidth, seed)
    val coefficients = frameLengths.flatMap { length =>
      (0 until length).map(index => window.map(_(index)))
    }
    val enabledBySample = Seq.fill(enabledSamples)(enable) ++
      Seq.fill(totalSamples - enabledSamples)(false)
    val expected = inputData.zip(coefficients).zip(enabledBySample).map {
      case ((data, coefficient), sampleEnabled) =>
        coefficient.filter(_ => sampleEnabled).map(windowModel(
          data,
          _,
          inputWidth,
          inputBinPoint,
          outputBinPoint,
          coeffWidth,
          coeffBinPoint,
          params.trimType
        )).getOrElse {
          val real = ArithmeticUtils.toSignedNBits(data >> inputWidth, inputWidth)
          val imag = ArithmeticUtils.toSignedNBits(data, inputWidth)
          val binaryPointShift = outputBinPoint - inputBinPoint
          (real << binaryPointShift, imag << binaryPointShift)
        }
    }

    window.filter(_ => runtimeRam).foreach {
      _.zipWithIndex.foreach { case (value, index) =>
        writeWord(
          ramAddress.base + beatBytes * index,
          WindowCoefficientQuantizer.quantize(value, coeffWidth, coeffBinPoint))
      }
    }

    poke(input.valid, false.B)
    poke(input.bits.last, false.B)
    poke(output.ready, false.B)
    step(1)
    val regs = Regs(beatBytes)
    writeWord(csrAddress.base + regs.ctrl, if (enable) BigInt(1) else BigInt(0))
    writeWord(csrAddress.base + regs.chirpsize, frameSize)

    val rng = new Random(seed ^ 0x5354414c4cL)
    var offered = false
    var written = 0
    var read = 0
    var cycles = 0
    var activeEnable = enable
    var heldOutput = Option.empty[(BigInt, BigInt)]
    val acceptedCycles = ArrayBuffer.empty[Int]
    val emittedCycles = ArrayBuffer.empty[Int]
    val watchdog = totalSamples * 50 + 100

    while (read < totalSamples && cycles < watchdog) {
      if (checkDisabled && activeEnable && written == enabledSamples && read == written) {
        poke(input.valid, false.B)
        writeWord(csrAddress.base + regs.ctrl, BigInt(0))
        step(1)
        activeEnable = false
      }
      val canOffer = !(checkDisabled && activeEnable && written >= enabledSamples)
      if (canOffer && !offered && written < totalSamples) {
        offered = !random || rng.nextBoolean()
      }
      poke(input.valid, offered)
      if (written < totalSamples) {
        poke(input.bits.data, inputData(written))
        poke(input.bits.last, lastIndices(written))
      }
      val ready = !random || rng.nextBoolean()
      poke(output.ready, ready)

      val outputValid = peek(output.valid) == 1
      val outputWord = peek(output.bits.data)
      val outputLast = peek(output.bits.last)
      heldOutput.foreach { held =>
        require(outputValid && held == ((outputWord, outputLast)),
          "Windowing output changed under backpressure")
      }
      heldOutput = if (outputValid && !ready) Some((outputWord, outputLast)) else None

      if (offered && peek(input.ready) == 1) {
        acceptedCycles += cycles
        written += 1
        offered = false
      }
      if (outputValid && ready) {
        emittedCycles += cycles
        require(read < written, "Windowing emitted data without an accepted input")
        val actual = (
          ArithmeticUtils.toSignedNBits(outputWord >> outputWidth, outputWidth),
          ArithmeticUtils.toSignedNBits(outputWord, outputWidth)
        )
        require(actual == expected(read),
          s"Windowing output[$read] $actual != ${expected(read)}")
        require(outputLast == (if (lastIndices(read)) 1 else 0),
          s"Windowing last mismatch at $read")
        if (verbose) {
          println(s"[$read] $actual")
        }
        read += 1
      }
      step(1)
      cycles += 1
    }

    poke(input.valid, false.B)
    poke(output.ready, true.B)
    require(read == totalSamples,
      s"Windowing watchdog expired after $cycles cycles ($read/$totalSamples outputs)")
    require(written == totalSamples,
      s"Windowing accepted only $written/$totalSamples inputs")
    step(2)
    if (!random && frameLengths.size == 1) {
      require(acceptedCycles.nonEmpty && acceptedCycles.size == emittedCycles.size,
        "Invalid Windowing throughput trace")
      require(acceptedCycles.sliding(2).forall(pair => pair(1) - pair(0) == 1),
        "Windowing did not accept one sample per cycle")
      require(emittedCycles.sliding(2).forall(pair => pair(1) - pair(0) == 1),
        "Windowing did not emit one sample per cycle")
    }
  }
}

class WindowingTLTester(
    dut: WindowingTL[FixedPoint] with StandaloneTLBlock,
    csrAddress: AddressSet,
    ramAddress: AddressSet,
    windowFuncRunTime: WindowType,
    params: WindowingParams[FixedPoint],
    beatBytes: Int,
    enable: Boolean = true,
    verbose: Boolean = false,
    random: Boolean = true,
    numFrames: Int = 2,
    shortFirstFrame: Boolean = false,
    checkDisabled: Boolean = false,
    seed: Long = 0x57494eL)
  extends PeekPokeTester(dut.module)
  with TLMasterModel
  with WindowingTestDriver {

  val mod: LazyModuleImp = dut.module
  override val memTL: TLBundle = dut.ioMem.get

  runWindowingTest(
    dut.in,
    dut.out,
    (address, data) => memWriteWord(address, data, beatBytes),
    csrAddress,
    ramAddress,
    windowFuncRunTime,
    params,
    beatBytes,
    enable,
    verbose,
    random,
    numFrames,
    shortFirstFrame,
    checkDisabled,
    seed)
}

class WindowingAXI4Tester(
    dut: WindowingAXI4[FixedPoint] with StandaloneAXI4Block,
    csrAddress: AddressSet,
    ramAddress: AddressSet,
    windowFuncRunTime: WindowType,
    params: WindowingParams[FixedPoint],
    beatBytes: Int = 4,
    enable: Boolean = true,
    verbose: Boolean = false,
    random: Boolean = true,
    numFrames: Int = 2,
    shortFirstFrame: Boolean = false,
    checkDisabled: Boolean = false,
    seed: Long = 0x57494eL)
  extends PeekPokeTester(dut.module)
  with AXI4MasterModel
  with WindowingTestDriver {

  val mod: LazyModuleImp = dut.module
  override val memAXI: AXI4Bundle = dut.ioMem.get

  runWindowingTest(
    dut.in,
    dut.out,
    (address, data) => memWriteWord(address, data),
    csrAddress,
    ramAddress,
    windowFuncRunTime,
    params,
    beatBytes,
    enable,
    verbose,
    random,
    numFrames,
    shortFirstFrame,
    checkDisabled,
    seed)
}
