package opera.fft

import chisel3._
import chisel3.util.log2Up
import chiseltest.iotesters.PeekPokeTester
import freechips.rocketchip.amba.axi4stream.AXI4StreamBundle

import scala.collection.mutable.ArrayBuffer
import ModelUtils.RawComplex

trait MemoryMappedFFTTestUtils { this: PeekPokeTester[_ <: Module] =>
  protected def csrRead(address: BigInt): BigInt
  protected def csrWrite(address: BigInt, data: BigInt): Unit
  protected def csrBaseAddress: BigInt
  protected def csrBeatBytes: Int
  protected def busName: String
  protected def runtimeFrameSeedBase: Long

  protected def runMemoryMappedCheck(in: AXI4StreamBundle, out: AXI4StreamBundle, params: FFTParams, mmCheck: MemoryMappedFFTCheck): Unit =
    mmCheck match {
      // Checks the output frame and the generation of the `last` signal for a static configuration.
      case StaticFrameCheck(inputData, expectedData, plotName) =>
        runComparison(
          in           = in,
          out          = out,
          params       = params,
          inputData    = inputData,
          expectedData = expectedData,
          frameSize    = params.fftSize,
          resetDut     = true,
          plotName     = Some(plotName)
        )
      // Writes runtime size, scaling, and direction CSRs, checks readback, then compares DUT against a matching model.
      case RuntimeConfigCheck =>
        final case class RuntimeCase(activeSize: Int, activeDivBy2Value: BigInt, direction: Boolean)

        val regs       = Regs(csrBeatBytes)
        val stageCount = log2Up(params.fftSize)
        def alternatingMask(width: Int, firstBitSet: Boolean): BigInt =
          (0 until width).foldLeft(BigInt(0)) { case (mask, bit) =>
            if (((bit & 1) == 0) == firstBitSet) mask | (BigInt(1) << bit) else mask
          }
        val cases      = Seq(
          RuntimeCase(activeSize = params.fftSize, activeDivBy2Value = BigInt(0x00), direction = true),
          RuntimeCase(activeSize =            256, activeDivBy2Value = alternatingMask(log2Up(256), firstBitSet = false), direction = false),
          RuntimeCase(activeSize =             64, activeDivBy2Value = alternatingMask(log2Up(64),  firstBitSet = true),  direction = false),
          RuntimeCase(activeSize =             16, activeDivBy2Value = alternatingMask(log2Up(16),  firstBitSet = false), direction = true),
          RuntimeCase(activeSize =              4, activeDivBy2Value = alternatingMask(log2Up(4),   firstBitSet = true),  direction = false),
          RuntimeCase(activeSize = params.fftSize, activeDivBy2Value = alternatingMask(stageCount,  firstBitSet = true),  direction = false),
          RuntimeCase(activeSize = params.fftSize, activeDivBy2Value = (BigInt(1) << stageCount) - 1, direction = true)
        )

        cases.zipWithIndex.foreach { case (config, index) =>
          val divBy2Value    = runtimeDivBy2Value(params, config.activeSize, config.activeDivBy2Value)
          val directionValue = if (config.direction) BigInt(1) else BigInt(0)
          val divBy2         = (0 until stageCount).map(stage => ((divBy2Value >> stage) & 1) == 1).toVector
          val runtimeFields  = Seq(
            ("size_log2", regs.sizeLog2, BigInt(log2Up(config.activeSize)), BigInt(0xFF)),
            ("divBy2", regs.divBy2, divBy2Value, (BigInt(1) << stageCount) - 1),
            ("direction", regs.direction, directionValue, BigInt(0x1))
          )

          reset(2)
          runtimeFields.foreach { case (_, offset, value, _) => csrWrite(csrBaseAddress + offset, value) }
          runtimeFields.foreach { case (name, offset, expected, mask) =>
            val peeked = csrRead(csrBaseAddress + offset) & mask
            TestLog.log(s"$busName csr $name: peeked=$peeked expected=$expected")
            require(peeked == expected, s"$name CSR readback mismatch")
          }
          csrWrite(csrBaseAddress + regs.loadCfg, 1)
          step(4)

          runRuntimeComparison(
            in,
            out,
            params,
            config.activeSize,
            divBy2,
            config.direction,
            runtimeFrameSeedBase + index,
            Some(s"$busName-runtime-${params.sdfRadix.label}-${params.decimation}-size-${config.activeSize}-div-${divBy2Value.toString(16)}-${if (config.direction) "fft" else "ifft"}")
          )
        }
      // Drives overflowing input data, checks the output frame, then verifies sticky overflow CSR set/clear behavior.
      case OverflowCsrCheck =>
        val regs          = Regs(csrBeatBytes)
        val maxRaw        = FFTModel.inputFormat(params).maxRaw
        val overflowFrame = Vector.fill(params.fftSize)(RawComplex(maxRaw, maxRaw))
        val inputData     = FFTModelTestUtils.repeatedDutInput(params, overflowFrame, frames = 4)
        val modelResult   = FFTModel(params, inputData)

        reset(2)

        require(modelResult.anyOverflow, "overflow wrapper input must exercise model overflow")
        runComparison(
          in            = in,
          out           = out,
          params        = params,
          inputData     = inputData,
          expectedData  = modelResult.checkedFrame(params.fftSize),
          frameSize     = params.fftSize,
          driveAllInput = true,
          plotName      = Some(s"$busName-overflow-${params.sdfRadix.label}-${params.decimation}-${params.fftSize}-max-value")
        )
        step(params.fftSize * 8)

        val overflowMask = (BigInt(1) << log2Up(params.fftSize)) - 1
        val overflow     = csrRead(csrBaseAddress + regs.overflow) & overflowMask
        TestLog.log(s"$busName csr overflow status: peeked=0x${overflow.toString(16)} expected=nonzero")
        require(overflow != 0, "expected sticky overflow CSR to latch at least one stage overflow")

        csrWrite(csrBaseAddress + regs.overflow, overflow)
        step(8)
        val cleared = csrRead(csrBaseAddress + regs.overflow) & overflowMask
        TestLog.log(s"$busName csr overflow after clear: peeked=0x${cleared.toString(16)} expected=0x0")
        require(cleared == 0, s"expected sticky overflow CSR to clear, got 0x${cleared.toString(16)}")
    }

  private def runtimeDivBy2Value(params: FFTParams, activeSize: Int, activeValue: BigInt): BigInt = {
    val maxStages    = log2Up(params.fftSize)
    val activeStages = log2Up(activeSize)
    val activeMask   = (BigInt(1) << activeStages) - 1
    val activeBits   = activeValue & activeMask
    if (params.decimation == DIF) activeBits << (maxStages - activeStages)
    else activeBits
  }

  // Builds static model params for one loaded runtime config, then compares DUT output against that model.
  private def runRuntimeComparison(
      in        : AXI4StreamBundle,
      out       : AXI4StreamBundle,
      params    : FFTParams,
      activeSize: Int,
      divBy2    : Seq[Boolean],
      direction : Boolean,
      seed      : Long,
      plotName  : Option[String] = None,
  ): Unit = {
    require(params.runTime, "active-size static params require a runtime FFT configuration")
    require(activeSize > 1 && (activeSize & (activeSize - 1)) == 0, s"activeSize must be a power of two, got $activeSize")
    require(activeSize <= params.fftSize, s"activeSize=$activeSize must fit within fftSize=${params.fftSize}")

    val maxStages    = log2Up(params.fftSize)
    val activeStages = log2Up(activeSize)
    require(divBy2.length == maxStages, s"divBy2 must contain $maxStages entries, got ${divBy2.length}")
    val divBy2ByStage = divBy2.toIndexedSeq
    val activeRange =
      if (params.decimation == DIF) (maxStages - activeStages) until maxStages
      else 0 until activeStages

    val activeModelParams = params.copy(
      fftSize          = activeSize,
      runTime          = false,
      divBy2Reg        = false,
      directionReg     = false,
      direction        = direction,
      inDataType       = params.stageInputType(activeRange.start),
      growEnable       = activeRange.map(params.stageGrowEnable).toSeq,
      divBy2           = activeRange.map(divBy2ByStage).toSeq,
      stageTrimTypes   = activeRange.map(params.resolvedStageTrimTypes).toSeq,
      twiddleTrimTypes = activeRange.map(params.resolvedTwiddleTrimTypes).toSeq,
    )
    val pattern = InputPatterns.multiTonePattern(
      activeSize,
      baseAmplitudeRaw = BigInt(96),
      noise            = Some(InputPatterns.FftNoise(maxAmplitudeRaw = 24, seed = seed)),
      label            = "multi-tone-noise"
    )
    val inputData    = FFTModelTestUtils.patternedDutInput(activeModelParams, pattern, frames = 3)
    val outputFormat = FFTModel.fftOutputFormat(params)
    val expectedData = FFTModel(activeModelParams, inputData).checkedFrame(activeSize).map(_.map(outputFormat.wrap))

    runComparison(
      in           = in,
      out          = out,
      params       = params,
      inputData    = inputData,
      expectedData = expectedData,
      frameSize    = activeSize,
      plotName     = plotName
    )
  }

  // Streams packed complex samples through the wrapper and checks output data and `last` signal.
  private def runComparison(
      in           : AXI4StreamBundle,
      out          : AXI4StreamBundle,
      params       : FFTParams,
      inputData    : Vector[RawComplex],
      expectedData : Vector[RawComplex],
      frameSize    : Int,
      resetDut     : Boolean = false,
      driveAllInput: Boolean = false,
      plotName     : Option[String] = None,
  ): Unit = {
    val inputWidth   = params.inDataType.real.getWidth
    val inputMask    = (BigInt(1) << inputWidth) - 1
    val outputFormat = FFTModel.fftOutputFormat(params)
    val outputWidth  = outputFormat.width
    val outputMask   = (BigInt(1) << outputWidth) - 1
    var writeCounter = 0
    var readCounter  = 0
    var cycles       = 0
    val peekedData   = ArrayBuffer.empty[RawComplex]
    val maxCycles    = 100 * frameSize + inputData.length + 4096

    TestLog.log(s"\n== FFT wrapper $busName ${plotName.getOrElse("comparison")} frameSize=$frameSize ==")

    if (resetDut) reset(2)
    poke(in.valid, 0)
    poke(in.bits.data, 0)
    poke(in.bits.last, 0)
    poke(out.ready, 0)
    step(1)
    poke(out.ready, 1)

    while ((readCounter < expectedData.length || (driveAllInput && writeCounter < inputData.length)) && cycles < maxCycles) {
      if (writeCounter < inputData.length) {
        val inputSample = inputData(writeCounter)
        poke(in.valid, 1)
        poke(in.bits.data, ((inputSample.real & inputMask) << inputWidth) | (inputSample.imag & inputMask))
        poke(in.bits.last, if ((writeCounter % frameSize) == frameSize - 1) 1 else 0)
      } else {
        poke(in.valid, 0)
        poke(in.bits.last, 0)
      }

      if (writeCounter < inputData.length && peek(in.ready) == 1) writeCounter += 1

      if (peek(out.valid) == 1 && readCounter < expectedData.length) {
        val peekedRaw = peek(out.bits.data)
        val peekedSample = RawComplex(
          outputFormat.wrap((peekedRaw >> outputWidth) & outputMask),
          outputFormat.wrap(peekedRaw & outputMask)
        )
        val expectedSample = expectedData(readCounter)
        val frameSampleIndex = readCounter % frameSize
        val expectedLast = if (frameSampleIndex == frameSize - 1) BigInt(1) else BigInt(0)
        val peekedLast = peek(out.bits.last)
        TestLog.log(
          f"sample=$readCounter%5d " +
            s"expectedRaw=(${expectedSample.real}, ${expectedSample.imag}) " +
            s"peekedRaw=(${peekedSample.real}, ${peekedSample.imag}) " +
            s"expectedLast=$expectedLast peekedLast=$peekedLast"
        )
        require(peekedSample == expectedSample, s"raw wrapper output mismatch at sample $readCounter: expected data=$expectedSample peeked data=$peekedSample")
        require(peekedLast == expectedLast, s"wrapper last mismatch at sample $readCounter: expected last=$expectedLast peeked last=$peekedLast")
        peekedData += peekedSample
        readCounter += 1
      }

      cycles += 1
      step(1)
    }

    require(
      readCounter == expectedData.length,
      s"checked $readCounter of ${expectedData.length} expected wrapper outputs after $cycles/$maxCycles cycles"
    )
    plotName.foreach { name =>
      PlotUtils
        .writePlotIfEnabled(
          name,
          peekedData.toVector.map(ModelUtils.rawToComplex(outputFormat, _)),
          expectedData.take(peekedData.length).map(ModelUtils.rawToComplex(outputFormat, _)),
          modelLabel = "peeked",
          breezeLabel = "expected"
        )
        .foreach(output => println(s"wrote wrapper FFT plot to ${output.getAbsolutePath}"))
    }
    poke(in.valid, 0)
    poke(in.bits.last, 0)
    step(4)
  }
}
