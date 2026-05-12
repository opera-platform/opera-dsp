package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

private class CFAROutputFrame[T <: Data: Real](params: CFARParams[T]) extends Bundle {
  val peak = Bool()
  val cut = if (params.sendCut) Some(params.inputType.cloneType) else None
  val threshold = params.thresholdType.cloneType
  val last = Bool()
  val fftBin = UInt(log2Ceil(params.maxFftSize).W)
}

class CellAveragingCFAR[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private def shiftRegisterWithClear(in: Bool, depth: Int, clear: Bool): Bool = {
    if (depth == 0) {
      in
    } else {
      val regs = RegInit(VecInit(Seq.fill(depth)(false.B)))
      when(clear) {
        regs.foreach(_ := false.B)
      }.otherwise {
        regs.head := in
        regs.tail.zip(regs.init).foreach { case (next, previous) => next := previous }
      }
      regs.last
    }
  }

  private def selectRuntimeTap[A <: Data](taps: Vec[A], depth: UInt): A = {
    Mux1H((1 to taps.length).map { depthValue =>
      (depth === depthValue.U) -> taps(depthValue - 1)
    })
  }

  private val thresholdPipeStages =
    if (params.runtimeLogMode) params.addPipeStages.max(params.mulPipeStages)
    else if (params.logMode) params.addPipeStages
    else params.mulPipeStages
  private val retimingStages = if (params.retiming) 1 else 0
  private val outputDelayStages = thresholdPipeStages + retimingStages
  private val outputQueueDepth = if (params.retiming) thresholdPipeStages + 1 else thresholdPipeStages

  val inputCount = RegInit(0.U(log2Ceil(params.maxFftSize).W))
  val outputCount = RegInit(0.U(log2Ceil(params.maxFftSize).W))
  val initialInputDone = RegInit(false.B)
  val flushing = RegInit(false.B)
  val lastCutDraining = RegInit(false.B)

  val activeDelay = io.i_reference_cells +& io.i_guard_cells +& 1.U

  assert(
    io.i_fft_size > 2.U * io.i_reference_cells + 2.U * io.i_guard_cells + 1.U,
    "FFT size must be larger than the active CFAR reference, guard, and CUT cells"
  )
  assert(io.i_reference_cells > 0.U, "Number of reference cells must be greater than zero")
  assert(io.i_guard_cells > 0.U, "Number of guard cells must be greater than zero")

  val sumType = (io.i_data.bits * log2Ceil(params.maxReferenceCells)).cloneType
  val referenceSumLeft = RegInit(0.U.asTypeOf(sumType))
  val referenceSumRight = RegInit(0.U.asTypeOf(sumType))

  val leftReferenceDelay =
    Module(new ReferenceDelayCells(params.inputType, params.maxReferenceCells, params.minSRAMDepth))
  leftReferenceDelay.io.i_data <> io.i_data
  leftReferenceDelay.io.i_depth := io.i_reference_cells
  leftReferenceDelay.io.i_last := io.i_last

  val leftGuardDelay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  leftGuardDelay.io.i_data <> leftReferenceDelay.io.o_data
  leftGuardDelay.io.i_depth := io.i_guard_cells
  leftGuardDelay.io.i_last := leftReferenceDelay.io.o_last

  val cutDelay = Module(new CFARCutDelay(params.inputType.cloneType))
  cutDelay.io.i_data <> leftGuardDelay.io.o_data
  cutDelay.io.i_last := leftGuardDelay.io.o_last

  val lastAfterThresholdDelay = ShiftRegister(cutDelay.io.o_last, outputQueueDepth, true.B)
  private val outputQueue = Module(new Queue(new CFAROutputFrame(params), outputQueueDepth + 1, flow = true, pipe = true))
  private val outputReady = outputQueue.io.enq.ready && io.o_data.ready

  val rightGuardDelay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  rightGuardDelay.io.i_data <> cutDelay.io.o_data
  rightGuardDelay.io.i_depth := io.i_guard_cells
  rightGuardDelay.io.i_last := cutDelay.io.o_last
  rightGuardDelay.io.o_data.ready := outputReady

  val rightReferenceDelay =
    Module(new ReferenceDelayCells(params.inputType, params.maxReferenceCells, params.minSRAMDepth))
  rightReferenceDelay.io.i_data <> rightGuardDelay.io.o_data
  rightReferenceDelay.io.i_depth := io.i_reference_cells
  rightReferenceDelay.io.i_last := rightGuardDelay.io.o_last
  rightReferenceDelay.io.o_data.ready := outputReady

  cutDelay.io.o_data.ready := Mux(rightReferenceDelay.io.o_full, rightReferenceDelay.io.i_data.ready, outputReady)

  val rawOutputFire = outputQueue.io.enq.fire
  val rawOutputLastFire = rawOutputFire && lastAfterThresholdDelay

  when(io.i_data.fire) {
    inputCount := inputCount + 1.U
  }

  when(inputCount === activeDelay - 1.U && io.i_data.fire) {
    initialInputDone := true.B
  }

  when(rawOutputLastFire) {
    inputCount := 0.U
  }

  when(rawOutputFire) {
    outputCount := outputCount + 1.U
  }

  when((outputCount === io.i_fft_size - 1.U && rawOutputFire) || rawOutputLastFire) {
    outputCount := 0.U
  }

  when(io.i_last && io.i_data.fire) {
    flushing := true.B
  }

  val flushingDelayed = shiftRegisterWithClear(flushing, outputDelayStages, rawOutputLastFire)

  when(rawOutputLastFire) {
    flushing := false.B
    initialInputDone := false.B
    lastCutDraining := true.B
  }

  when(rightReferenceDelay.io.o_empty) {
    lastCutDraining := false.B
  }

  when(rawOutputLastFire) {
    referenceSumLeft := 0.U.asTypeOf(sumType)
  }.elsewhen(io.i_data.fire) {
    when(leftReferenceDelay.io.o_full) {
      when(leftReferenceDelay.io.o_data.fire) {
        referenceSumLeft := referenceSumLeft + leftReferenceDelay.io.i_data.bits - leftReferenceDelay.io.o_data.bits
      }
    }.otherwise {
      referenceSumLeft := referenceSumLeft + leftReferenceDelay.io.i_data.bits
    }
  }

  when(lastCutDraining) {
    referenceSumRight := 0.U.asTypeOf(sumType)
  }.elsewhen(rightReferenceDelay.io.i_data.fire) {
    when(rightReferenceDelay.io.o_full) {
      when(rightReferenceDelay.io.o_data.fire) {
        referenceSumRight := referenceSumRight + rightReferenceDelay.io.i_data.bits - rightReferenceDelay.io.o_data.bits
      }
    }.otherwise {
      referenceSumRight := referenceSumRight + rightReferenceDelay.io.i_data.bits
    }
  }

  val leftAverage = BinaryRepresentation[T].shr(referenceSumLeft, io.i_noise_div_shift)
  val rightAverage = BinaryRepresentation[T].shr(referenceSumRight, io.i_noise_div_shift)
  val greatestOf = Mux(leftAverage > rightAverage, leftAverage, rightAverage).asTypeOf(leftAverage.cloneType)
  val smallestOf = Mux(leftAverage < rightAverage, leftAverage, rightAverage).asTypeOf(leftAverage.cloneType)
  val cellAverage = BinaryRepresentation[T].shr(rightAverage + leftAverage, 1).asTypeOf(leftAverage.cloneType)

  val modeAverage = MuxLookup(io.i_cfar_mode, smallestOf)(Seq(
    CFARMode.CellAveraging.U -> cellAverage,
    CFARMode.GreatestOf.U -> greatestOf,
    CFARMode.SmallestOf.U -> smallestOf
  ))
  val zeroAverage = 0.U.asTypeOf(modeAverage.cloneType)

  val rightAverageEnabled = RegInit(false.B)
  when(!leftReferenceDelay.io.o_full && leftReferenceDelay.io.o_data.fire) {
    rightAverageEnabled := true.B
  }
  when(rawOutputLastFire) {
    rightAverageEnabled := false.B
  }

  val averageBeforeScaling =
    if (params.retiming) {
      RegNext(
        Mux(
          !rightReferenceDelay.io.o_full && !leftReferenceDelay.io.o_full,
          zeroAverage,
          Mux(
            leftReferenceDelay.io.o_full && !rightReferenceDelay.io.o_full,
            leftAverage,
            Mux(rightAverageEnabled, rightAverage, modeAverage)
          )
        )
      )
    } else {
      Mux(
        !rightReferenceDelay.io.o_full && !leftReferenceDelay.io.o_full,
        zeroAverage,
        Mux(
          leftReferenceDelay.io.o_full && !rightReferenceDelay.io.o_full,
          leftAverage,
          Mux(rightAverageEnabled, rightAverage, modeAverage)
        )
      )
    }

  val threshold =
    if (params.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(
        numAddPipes = thresholdPipeStages,
        numMulPipes = thresholdPipeStages
      )) {
        val logThreshold = Wire(params.thresholdType.cloneType)
        val linearThreshold = Wire(params.thresholdType.cloneType)
        logThreshold := averageBeforeScaling context_+ io.i_threshold_scale
        linearThreshold := averageBeforeScaling context_* io.i_threshold_scale
        Mux(io.i_log_mode.get, logThreshold, linearThreshold)
      }
    } else if (params.logMode) {
      DspContext.withNumAddPipes(params.addPipeStages) {
        averageBeforeScaling context_+ io.i_threshold_scale
      }
    } else {
      DspContext.withNumMulPipes(params.mulPipeStages) {
        averageBeforeScaling context_* io.i_threshold_scale
      }
    }

  val cut = ShiftRegister(cutDelay.io.o_data.bits, outputQueueDepth, true.B)
  val leftNeighbor = ShiftRegister(selectRuntimeTap(leftGuardDelay.io.o_taps, io.i_guard_cells), outputQueueDepth, true.B)
  val rightNeighbor = ShiftRegister(leftGuardDelay.io.o_taps.head, outputQueueDepth, true.B)
  val isLocalMaximum = cut > leftNeighbor && cut > rightNeighbor
  val isAboveThreshold = cut > threshold
  val peak = Mux(io.i_peak_grouping, isAboveThreshold && isLocalMaximum, isAboveThreshold)

  val rawOutputValid =
    if (outputDelayStages == 0) {
      (initialInputDone && io.i_data.fire) || flushing
    } else {
      ShiftRegister(initialInputDone && io.i_data.fire, outputDelayStages, true.B) ||
        (flushingDelayed && ShiftRegister(outputReady, outputDelayStages, true.B))
    }

  val fillingInputWindow = !initialInputDone && inputCount < activeDelay
  io.i_data.ready := fillingInputWindow || outputReady && !flushingDelayed

  outputQueue.io.enq.valid := rawOutputValid
  outputQueue.io.enq.bits.peak := peak
  outputQueue.io.enq.bits.threshold := threshold
  outputQueue.io.enq.bits.last := lastAfterThresholdDelay
  outputQueue.io.enq.bits.fftBin := outputCount
  if (params.sendCut) {
    outputQueue.io.enq.bits.cut.get := cut
  }

  outputQueue.io.deq.ready := io.o_data.ready

  io.o_data.valid := outputQueue.io.deq.valid
  io.o_data.bits.peak := outputQueue.io.deq.bits.peak
  io.o_data.bits.threshold := outputQueue.io.deq.bits.threshold
  if (params.sendCut) {
    io.o_data.bits.cut.get := outputQueue.io.deq.bits.cut.get
  }
  io.o_last := outputQueue.io.deq.bits.last
  io.o_fft_bin := outputQueue.io.deq.bits.fftBin
}
