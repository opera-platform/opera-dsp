package opera.cfar

import chisel3._
import chisel3.util.log2Ceil
import chiseltest.iotesters.PeekPokeTester
import dsptools.numbers.{BinaryRepresentation, Real}
import fixedpoint._
import freechips.rocketchip.amba.axi4stream.AXI4StreamBundle

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait MemoryMappedCFARTestUtils { this: PeekPokeTester[_ <: Module] =>
  protected def csrRead(address: BigInt): BigInt
  protected def csrWrite(address: BigInt, data: BigInt): Unit
  protected def csrBaseAddress: BigInt
  protected def csrBeatBytes: Int
  protected def busName: String

  protected def runMemoryMappedCheck[T <: Data: Real: BinaryRepresentation](
      in     : AXI4StreamBundle,
      out    : AXI4StreamBundle,
      params : CFARParams[T],
      mmCheck: MemoryMappedCFARCheck
  ): Unit =
    mmCheck match {
      case SingleFrameSweepCheck(frames) =>
        runSingleFrameCases(in, out, params, frames)
      case TwoFrameReconfigCheck(first, second) =>
        runTwoFrameReconfigCase(in, out, params, first, second)
      case MidFramePendingConfigCheck(first, pending, second, updateAfterAcceptedIndex) =>
        runMidFramePendingConfigCase(in, out, params, first, pending, second, updateAfterAcceptedIndex)
    }

  private def runSingleFrameCases[T <: Data: Real: BinaryRepresentation](
      in    : AXI4StreamBundle,
      out   : AXI4StreamBundle,
      params: CFARParams[T],
      frames: Seq[MemoryMappedCFARFrameCase]
  ): Unit = {
    require(frames.nonEmpty, "Memory-mapped CFAR sweep must contain at least one frame case")
    reset(2)
    frames.foreach { frame =>
      configure(params, frame.config, fftSize = frame.fftSize, checkReadback = true)
      runFrame(in, out, params, frame)
    }
  }

  private def runTwoFrameReconfigCase[T <: Data: Real: BinaryRepresentation](
      in    : AXI4StreamBundle,
      out   : AXI4StreamBundle,
      params: CFARParams[T],
      first : MemoryMappedCFARFrameCase,
      second: MemoryMappedCFARFrameCase
  ): Unit = {
    reset(2)
    configure(params, first.config, fftSize = first.fftSize, checkReadback = true)
    runFrame(in, out, params, first)
    configure(params, second.config, fftSize = second.fftSize, checkReadback = true)
    runFrame(in, out, params, second)
  }

  private def runMidFramePendingConfigCase[T <: Data: Real: BinaryRepresentation](
      in                      : AXI4StreamBundle,
      out                     : AXI4StreamBundle,
      params                  : CFARParams[T],
      first                   : MemoryMappedCFARFrameCase,
      pending                 : MemoryMappedCFARFrameCase,
      second                  : MemoryMappedCFARFrameCase,
      updateAfterAcceptedIndex: Int
  ): Unit = {
    require(first.fftSize == pending.fftSize, "mid-frame pending config keeps the active frame size stable")
    reset(2)
    configure(params, first.config, fftSize = first.fftSize, checkReadback = true)
    runFrame(
      in = in,
      out = out,
      params = params,
      frame = first,
      onInputAccepted = { sampleIndex =>
        if (sampleIndex == updateAfterAcceptedIndex) {
          configure(params, pending.config, fftSize = pending.fftSize, checkReadback = true)
        }
      }
    )
    runFrame(in, out, params, second)
  }

  private def runFrame[T <: Data: Real: BinaryRepresentation](
      in             : AXI4StreamBundle,
      out            : AXI4StreamBundle,
      params         : CFARParams[T],
      frame          : MemoryMappedCFARFrameCase,
      onInputAccepted: Int => Unit = (_: Int) => ()
  ): Unit = {
    val inputFrame = dataFor(frame)
    streamAndCheck(
      in = in,
      out = out,
      params = params,
      frame = inputFrame,
      expectedBins = expectedFor(params, inputFrame, frame.config),
      readyPattern = frame.readyPattern,
      randomReadyValidSeed = effectiveRandomReadyValidSeed(frame),
      plotName = frame.plotName,
      onInputAccepted = onInputAccepted
    )
  }

  private def expectedFor[T <: Data: Real](
      params: CFARParams[T],
      data  : Seq[Double],
      cfg   : MemoryMappedCFARRuntimeConfig
  ): Seq[CFARModel.ExpectedBin] =
    if (params.cfarType == CFARType.OrderedStatistic) {
      CFARModel.expectedGOSFrame(
        params = params,
        data = data,
        cfarMode = cfg.mode,
        thresholdScale = cfg.thresholdScale,
        logMode = cfg.logMode,
        referenceCells = cfg.referenceCells,
        guardCells = cfg.guardCells,
        orderRankLeft = cfg.orderRankLeft,
        orderRankRight = cfg.orderRankRight,
        edgePolicy = cfg.edgePolicy
      )
    } else {
      CFARModel.expectedFrame(
        params = params,
        data = data,
        cfarMode = cfg.mode,
        thresholdScale = cfg.thresholdScale,
        logMode = cfg.logMode,
        referenceCells = cfg.referenceCells,
        guardCells = cfg.guardCells,
        edgePolicy = cfg.edgePolicy
      )
    }

  private def dataFor(frame: MemoryMappedCFARFrameCase): Seq[Double] = frame.inputData.getOrElse {
    val random = new Random(frame.dataSeed)
    val phase = (frame.dataSeed & 0x7L).toInt
    (0 until frame.fftSize).map { index =>
      val background = random.nextInt(24) + (index % 5)
      val shapedPeak = if (((index + phase) % 11) == 0) 18 else 0
      (background + shapedPeak).toDouble / 64.0
    }
  }

  private def effectiveRandomReadyValidSeed(frame: MemoryMappedCFARFrameCase): Option[Long] =
    frame.randomReadyValidSeed.orElse {
      if (frame.randomReadyValid && TestConfig.randomReadyValid) Some(frame.defaultRandomReadyValidSeed) else None
    }

  private def configure[T <: Data: Real](
      params       : CFARParams[T],
      cfg          : MemoryMappedCFARRuntimeConfig,
      fftSize      : Int,
      checkReadback: Boolean
  ): Unit = {
    require(fftSize <= params.maxFftSize, s"active fftSize=$fftSize exceeds maxFftSize=${params.maxFftSize}")
    require(cfg.referenceCells <= params.maxReferenceCells, s"referenceCells=${cfg.referenceCells} exceeds maxReferenceCells=${params.maxReferenceCells}")
    require(cfg.guardCells <= params.maxGuardCells, s"guardCells=${cfg.guardCells} exceeds maxGuardCells=${params.maxGuardCells}")
    if (!params.runtimeLogMode) {
      require(cfg.logMode == params.logMode, "static logMode params require matching test config")
    }
    if (!params.runtimeEdgePolicy) {
      require(cfg.edgePolicy == params.edgePolicy, "static edgePolicy params require matching test config")
    }

    val regs = CFARRegs(csrBeatBytes)
    val thresholdScaleRaw = rawForDouble(cfg.thresholdScale, params.scaleType)
    val noiseDivShift = log2Ceil(cfg.referenceCells)
    val fields = Seq(
      regs.fftSize -> BigInt(fftSize),
      regs.thresholdScale -> thresholdScaleRaw,
      regs.peakGrouping -> (if (cfg.peakGrouping) BigInt(1) else BigInt(0)),
      regs.cfarMode -> BigInt(cfg.mode.value),
      regs.referenceCells -> BigInt(cfg.referenceCells),
      regs.guardCells -> BigInt(cfg.guardCells)
    ) ++ (
      if (params.cfarType == CFARType.OrderedStatistic) {
        Seq(
          regs.orderRankLeft -> BigInt(cfg.orderRankLeft),
          regs.orderRankRight -> BigInt(cfg.orderRankRight)
        )
      } else {
        Seq(regs.noiseDivShift -> BigInt(noiseDivShift))
      }
    ) ++ (
      if (params.runtimeLogMode) Seq(regs.logMode -> (if (cfg.logMode) BigInt(1) else BigInt(0))) else Seq.empty
    ) ++ (
      if (params.runtimeEdgePolicy) Seq(regs.edgePolicy -> BigInt(cfg.edgePolicy)) else Seq.empty
    )

    fields.foreach { case (offset, value) => csrWrite(csrBaseAddress + offset, value) }
    if (checkReadback) {
      fields.foreach { case (offset, expected) =>
        val width = fieldWidth(params, offset, regs)
        val mask = bitMask(width)
        val peeked = csrRead(csrBaseAddress + offset) & mask
        require(
          peeked == (expected & mask),
          s"$busName CFAR CSR readback mismatch at offset 0x${offset.toHexString}: expected=0x${(expected & mask).toString(16)} actual=0x${peeked.toString(16)}"
        )
      }
    }
    csrWrite(csrBaseAddress + regs.loadCfg, 1)
    step(4)
  }

  private def fieldWidth[T <: Data: Real](params: CFARParams[T], offset: Int, regs: CFARRegs): Int = {
    if (offset == regs.fftSize) log2Ceil(params.maxFftSize + 1)
    else if (offset == regs.thresholdScale) params.scaleType.getWidth
    else if (offset == regs.peakGrouping) 1
    else if (offset == regs.cfarMode) 2
    else if (offset == regs.referenceCells) log2Ceil(params.maxReferenceCells + 1)
    else if (offset == regs.guardCells) log2Ceil(params.maxGuardCells + 1)
    else if (offset == regs.noiseDivShift) log2Ceil(log2Ceil(params.maxReferenceCells + 1))
    else if (offset == regs.orderRankLeft || offset == regs.orderRankRight) log2Ceil(params.maxReferenceCells + 1)
    else if (offset == regs.logMode) 1
    else if (offset == regs.edgePolicy) 2
    else csrBeatBytes * 8
  }

  private def streamAndCheck[T <: Data: Real](
      in                  : AXI4StreamBundle,
      out                 : AXI4StreamBundle,
      params              : CFARParams[T],
      frame               : Seq[Double],
      expectedBins        : Seq[CFARModel.ExpectedBin],
      readyPattern        : Seq[Boolean],
      randomReadyValidSeed: Option[Long],
      plotName            : Option[String],
      onInputAccepted     : Int => Unit
  ): Unit = {
    require(frame.length == expectedBins.length, "CFAR memory-mapped test data and expected outputs must have the same length")
    require(readyPattern.nonEmpty, "readyPattern must not be empty")

    val random = randomReadyValidSeed.map(new Random(_))
    val inputWidth = params.inputType.getWidth
    val payloadWidth = params.thresholdType.getWidth + log2Ceil(params.maxFftSize) + 1 +
      (if (params.sendCut) params.inputType.getWidth else 0)
    val payloadMask = bitMask(payloadWidth)
    var inputIndex = 0
    var outputIndex = 0
    var cycle = 0
    var pendingInput: Option[Int] = None
    var heldOutput: Option[(BigInt, BigInt)] = None
    val plotSamples = ArrayBuffer.empty[PlotSample]
    val maxCycles = if (random.isDefined) frame.length * 800 + 8192 else frame.length * 300 + 4096

    poke(in.valid, 0)
    poke(in.bits.data, 0)
    poke(in.bits.last, 0)
    poke(out.ready, 0)
    step(1)

    while (outputIndex < expectedBins.length && cycle < maxCycles) {
      val outputReady =
        random.map(_.nextInt(4) != 0).getOrElse(readyPattern.lift(cycle).getOrElse(readyPattern.last))
      poke(out.ready, if (outputReady) 1 else 0)

      if (pendingInput.isEmpty && inputIndex < frame.length && random.forall(_.nextInt(5) != 0)) {
        pendingInput = Some(inputIndex)
      }
      pendingInput match {
        case Some(index) =>
          poke(in.valid, 1)
          poke(in.bits.data, unsigned(expectedBins(index).cut.raw, inputWidth))
          poke(in.bits.last, if (index == frame.length - 1) 1 else 0)
        case None =>
          poke(in.valid, 0)
          poke(in.bits.data, 0)
          poke(in.bits.last, 0)
      }

      val acceptedInput = if (pendingInput.nonEmpty && peek(in.ready) == 1) pendingInput else None
      if (peek(out.valid) == 1) {
        val current = (peek(out.bits.data) & payloadMask, peek(out.bits.last))
        heldOutput.foreach { previous =>
          require(
            current == previous,
            s"$busName CFAR output changed while backpressured: previous=$previous current=$current"
          )
        }

        if (outputReady) {
          val expectedPayload = packedPayload(params, expectedBins(outputIndex), outputIndex) & payloadMask
          val expectedLast = if (outputIndex == expectedBins.length - 1) BigInt(1) else BigInt(0)
          require(
            current._1 == expectedPayload,
            s"$busName CFAR packed output mismatch at sample $outputIndex: expected=0x${expectedPayload.toString(16)} actual=0x${current._1.toString(16)} trace=${expectedBins(outputIndex).trace}"
          )
          require(
            current._2 == expectedLast,
            s"$busName CFAR last mismatch at sample $outputIndex: expected=$expectedLast actual=${current._2}"
          )
          plotSamples += plotSample(params, current._1)
          outputIndex += 1
          heldOutput = None
        } else if (heldOutput.isEmpty) {
          heldOutput = Some(current)
        }
      } else if (heldOutput.nonEmpty) {
        throw new RuntimeException(s"$busName CFAR out.valid dropped while backpressured: held=${heldOutput.get}")
      }

      cycle += 1
      step(1)
      acceptedInput.foreach { acceptedIndex =>
        inputIndex += 1
        pendingInput = None
        poke(in.valid, 0)
        poke(in.bits.last, 0)
        poke(out.ready, 0)
        onInputAccepted(acceptedIndex)
      }
    }

    require(outputIndex == expectedBins.length, s"$busName CFAR observed $outputIndex of ${expectedBins.length} outputs after $cycle/$maxCycles cycles")
    plotName.foreach { name =>
      PlotUtils
        .writePlotIfEnabled(name, plotSamples.toVector)
        .foreach(output => println(s"wrote memory-mapped CFAR plot to ${output.getAbsolutePath}"))
    }
    poke(in.valid, 0)
    poke(in.bits.last, 0)
    poke(out.ready, 0)
    step(4)
  }

  private def packedPayload[T <: Data: Real](params: CFARParams[T], bin: CFARModel.ExpectedBin, fftBin: Int): BigInt = {
    val threshold = unsigned(bin.threshold.raw, params.thresholdType.getWidth)
    val peak = if (bin.peak) BigInt(1) else BigInt(0)
    val binWidth = log2Ceil(params.maxFftSize)
    if (params.sendCut) {
      val cut = unsigned(bin.cut.raw, params.inputType.getWidth)
      (((threshold << params.inputType.getWidth) | cut) << binWidth | BigInt(fftBin)) << 1 | peak
    } else {
      (threshold << binWidth | BigInt(fftBin)) << 1 | peak
    }
  }

  private def plotSample[T <: Data: Real](params: CFARParams[T], payload: BigInt): PlotSample = {
    val binWidth = log2Ceil(params.maxFftSize)
    val peak = (payload & 1) == 1
    val fftBin = ((payload >> 1) & bitMask(binWidth)).toInt
    val dataOffset = 1 + binWidth
    val (cut, thresholdRaw) =
      if (params.sendCut) {
        val cutRaw = (payload >> dataOffset) & bitMask(params.inputType.getWidth)
        val threshold = (payload >> (dataOffset + params.inputType.getWidth)) & bitMask(params.thresholdType.getWidth)
        (Some(rawToDouble(cutRaw, params.inputType)), threshold)
      } else {
        (None, (payload >> dataOffset) & bitMask(params.thresholdType.getWidth))
      }

    PlotSample(
      fftBin = fftBin,
      cut = cut,
      threshold = rawToDouble(thresholdRaw, params.thresholdType),
      peak = peak
    )
  }

  private def rawForDouble(value: Double, dataType: Data): BigInt =
    dataType match {
      case fixed: FixedPoint =>
        require(fixed.binaryPoint.known, "FixedPoint binary point must be known for CFAR memory-mapped tests")
        BigInt(math.round(value * math.pow(2.0, fixed.binaryPoint.get.toDouble)))
      case _: UInt =>
        require(value >= 0.0, s"UInt test literal must be non-negative, got $value")
        BigInt(math.round(value))
      case _: SInt =>
        BigInt(math.round(value))
      case other =>
        throw new IllegalArgumentException(s"Unsupported CFAR memory-mapped test type: ${other.getClass.getName}")
    }

  private def rawToDouble(raw: BigInt, dataType: Data): Double =
    dataType match {
      case fixed: FixedPoint =>
        require(fixed.binaryPoint.known, "FixedPoint binary point must be known for CFAR plots")
        toSigned(raw, fixed.getWidth).toDouble / math.pow(2.0, fixed.binaryPoint.get.toDouble)
      case _: UInt =>
        raw.toDouble
      case sint: SInt =>
        toSigned(raw, sint.getWidth).toDouble
      case other =>
        throw new IllegalArgumentException(s"Unsupported CFAR plot type: ${other.getClass.getName}")
    }

  private def unsigned(raw: BigInt, width: Int): BigInt = raw & bitMask(width)

  private def toSigned(raw: BigInt, width: Int): BigInt = {
    val masked = raw & bitMask(width)
    val signBit = BigInt(1) << (width - 1)
    if (masked >= signBit) masked - (BigInt(1) << width) else masked
  }

  private def bitMask(width: Int): BigInt = (BigInt(1) << width) - 1
}
