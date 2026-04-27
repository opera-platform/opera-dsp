package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

// TODO: For output scaling maybe do div2 instead of truncation. More logic but more accurate
class StandaloneR22SDF[T <: Data: Real: BinaryRepresentation](val params: FFTParams[T]) extends Module with HasIO[T] {
  val io: FFTIO[T] = IO(new FFTIO(params))

  // Constants
  private val isItDIF        = params.decimation == DIF
  private val noOfStages     = log2Ceil(params.fftSize)
  private val evenNoOfStages = noOfStages % 2 == 0
  private val sortedStages   = if (isItDIF) (0 until noOfStages).reverse else 0 until noOfStages
  private val stageDelays    = sortedStages.map(i => 1 << i)
  private val noOfTwiddles   = (noOfStages - 1) / 2
  private val stageCountWidth = log2Ceil(noOfStages) + 1
  private val stageRoleIndices = (0 until noOfStages).map(i => if (isItDIF) i else noOfStages - i - 1)
  private val staticStageOdd = stageRoleIndices.map(i => (i & 1) == 1)
  private val stageHasTwiddleControl = (0 until noOfStages).map(i => if (isItDIF) i != noOfStages - 1 else i != 0)
  private val staticTwiddleLookupIndices = (0 until noOfStages).map { i =>
    if (isItDIF) i >> 1 else if (evenNoOfStages) (i - 1) >> 1 else i >> 1
  }

  // Latencies
  private val complexMulLatency = if (params.use4Muls) params.numAddPipes + params.numMulPipes else 2 * params.numAddPipes + params.numMulPipes
  private val outputLatency     = params.numAddPipes + complexMulLatency
  private val latency           = outputLatency * noOfStages

  // Registers
  private val r_num_stages      = if (params.runTime) Some(RegInit(noOfStages.U(stageCountWidth.W))) else None
  private val r_fft_or_ifft     = if (params.directionReg) Some(RegInit(params.direction.B)) else None
  private val r_counters_msb    = RegInit(VecInit(Seq.fill(noOfStages)(false.B)))
  private val inputSampleCount  = RegInit(0.U(log2Ceil(params.fftSize).W))
  private val acceptedFrameCount = RegInit(0.U(1.W))
  private val outputSampleCount = RegInit(0.U(log2Ceil(params.fftSize).W))

  // Wires
  private val w_output           = Wire(params.outDataType)
  private val w_stage_en         = Wire(Vec(noOfStages, Bool()))
  private val w_i_en_last_stage  = Wire(Bool())
  private val w_last_stage_valid = Wire(Bool())
  private val w_fft_size         = Wire(UInt(log2Ceil(params.fftSize + 1).W))
  private val w_stage_outputs    = Wire(Vec(noOfStages, params.outDataType))
  private val w_stage_tail       = Wire(params.outDataType)
  private val w_mul_outputs      = Wire(MixedVec((0 until noOfStages).map { i => params.protoIQstages(i) }))
  private val w_invert_signals   = Wire(Vec(noOfStages, Bool()))

  // Stage-activity flags
  private val runtimeStageOdd    = if (params.runTime) Some(Wire(Vec(noOfStages, Bool()))) else None
  private val runtimeStageActive = if (params.runTime) Some(Wire(Vec(noOfStages, Bool()))) else None

  // Twiddle factor infrastructure
  private val w_lookup_twiddles = Wire(Vec(noOfTwiddles, params.twiddleType))
  private val w_lookup_address  = Wire(Vec(noOfTwiddles, UInt(noOfStages.W)))
  private val w_twiddles        = Wire(Vec(noOfStages, params.twiddleType))
  private val w_twiddle_address = Wire(Vec(noOfStages, UInt(noOfStages.W)))
  private val LUT               = QuarterWaveSineLUT[T](1 << noOfStages, params.twiddleType)

  // Runtime-derived signals
  private val activeStageCount = r_num_stages.getOrElse(noOfStages.U(stageCountWidth.W))
  private val isShiftedAddress = if (params.runTime) activeStageCount(0) ^ (noOfStages & 1).B else false.B
  private val fftOrIfft        = r_fft_or_ifft.getOrElse(params.direction.B)

  w_fft_size := (if (params.runTime) 1.U << activeStageCount else params.fftSize.U)

  val inputFrameStart = io.in.fire && inputSampleCount === 0.U
  val inputFrameEnd   = io.in.fire && inputSampleCount === (w_fft_size - 1.U)
  val sdfWarm         = acceptedFrameCount =/= 0.U

  when(inputFrameStart) {
    if (params.runTime)      r_num_stages.get  := io.i_size.get
    if (params.directionReg) r_fft_or_ifft.get := io.i_fft_or_ifft.get
  }

  when(inputFrameEnd) {
    inputSampleCount := 0.U
    acceptedFrameCount := 1.U
  }.elsewhen(io.in.fire) {
    inputSampleCount := inputSampleCount + 1.U
  }

  w_i_en_last_stage := (if (params.runTime && !isItDIF)
    w_stage_en((activeStageCount - 1.U).asTypeOf(UInt(log2Ceil(noOfStages).W))) else w_stage_en(noOfStages - 1))

  // Stage-activity computation
  if (params.runTime) {
    runtimeStageActive.get.zip(runtimeStageOdd.get).zipWithIndex.foreach {
      case ((active, odd), i) =>
        val index = stageRoleIndices(i)
        active := noOfStages.U - activeStageCount <= index.U
        odd    := (index.U - (noOfStages.U - activeStageCount))(0)
    }
  }

  // Twiddle LUT wiring
  w_lookup_address.zipWithIndex.foreach { case (address, i) => address := selectedLookupAddress(i) }
  (0 until noOfStages by 2).dropRight(1).zipWithIndex.foreach {
    case (m, i) =>
      val stageN = 1 << (noOfStages - m)
      val j = if (isItDIF) i else noOfTwiddles - 1 - i
      w_lookup_twiddles(j) := TwiddleFromLUT[T](w_lookup_address(j)(log2Ceil(stageN) - 1, 0), stageN, 1 << noOfStages, LUT)
  }

  // Stage instantiation and twiddle address generation
  val sdf_stages: Seq[R22SDF[T]] = stageDelays.zipWithIndex.map {
    case (delay, i) =>
      val stageParams = params.copy(inDataType = params.protoIQstages(i))
      val stage = Module(new R22SDF(RadixParams(
        dataType      = stageParams.inDataType,
        twiddleType   = stageParams.twiddleType,
        stageSize     = if (isItDIF) params.fftSize >> i else 2 << i,
        decimation    = stageParams.decimation,
        overflowReg   = stageParams.overflowReg,
        divBy2Reg     = stageParams.divBy2Reg,
        divBy2        = params.divBy2(i),
        growEnable    = stageParams.expandLogic(i) == 1,
        latency       = complexMulLatency,
        addPipeRegs   = params.numAddPipes,
        mulPipeRegs   = params.numMulPipes,
        dspMul4       = params.use4Muls,
        delay         = delay,
        bufferAsMem   = params.minSRAMdepth < delay,
        singlePortMem = params.singlePortSRAM,
        trimType      = stageParams.trimType,
      )))

      if (params.divBy2Reg)   stage.io.i_divBy2.get  := io.i_divBy2.get(i)
      if (params.overflowReg) io.o_overflow.get(i)   := stage.io.o_overflow.get

      w_stage_en(i) := stage.io.i_en

      // Twiddle address generation and inversion-signal logic
      wireStageTwiddleControl(stage, i, delay)
      stage
  }

  // Enable chain: stage 0 enabled by input, each stage enables next stage
  sdf_stages.scanLeft(io.in.fire) { case (en, s) => s.io.i_en := en; s.io.o_en }

  // Data path: connect stages in series, applying twiddle factors and/or inversion as needed
  val firstStageInput = if (params.runTime) Mux(runtimeStageActive.get(0), io.in.bits, 0.U.asTypeOf(params.inDataType)) else io.in.bits
  sdf_stages.map(_.io).zipWithIndex.foldLeft(firstStageInput) {
    case (prev_out, (stage, index)) =>
      w_stage_outputs(index) := stage.out
      val bpos = params.protoIQstages(index).real.cloneType match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _              => 0
      }
      if (isItDIF) connectDIFStage(stage, index, prev_out, bpos)
      else         connectDITStage(stage, index, prev_out, bpos)
  }

  if (isItDIF) w_stage_tail := ShiftRegister(w_stage_outputs.last, complexMulLatency, true.B)
  else if (params.runTime)
    w_stage_tail := w_stage_outputs((activeStageCount - 1.U).asTypeOf(UInt(log2Ceil(noOfStages).W)))
  else
    w_stage_tail := w_stage_outputs.last

  w_last_stage_valid := ShiftRegisterWithReset(
    sdfWarm && w_i_en_last_stage, outputLatency,
    resetData = false.B, reset = reset.asBool, en = true.B)

  io.in.ready := ~w_last_stage_valid
  private val shift_output = noOfStages.U * (params.expandLogic.sum != 0 || params.divBy2Reg).B
  // o_last is generated only from the number of accepted output samples.
  // It stays asserted with the final sample while the output is backpressured.
  val outputLast = outputSampleCount === (w_fft_size - 1.U)

  if (latency == 0) {
    io.out.valid := w_last_stage_valid
    Utils.assignFftOutputByDirection(w_stage_tail, w_output, fftOrIfft)
    when(io.out.fire) {
      outputSampleCount := Mux(outputLast, 0.U, outputSampleCount + 1.U)
    }
    io.o_last := io.out.valid && outputLast
  } else {
    val outQueue = Module(new Queue(chiselTypeOf(w_stage_tail), entries = latency + 1, pipe = true, flow = true))
    outQueue.io.enq.bits := w_stage_tail
    outQueue.io.enq.valid := w_last_stage_valid
    outQueue.io.deq.ready := io.out.ready

    io.in.ready  := !sdfWarm || outQueue.io.enq.ready
    io.out.valid := outQueue.io.deq.valid
    Utils.assignFftOutputByDirection(outQueue.io.deq.bits, w_output, fftOrIfft)
    when(io.out.fire) {
      outputSampleCount := Mux(outputLast, 0.U, outputSampleCount + 1.U)
    }
    io.o_last := io.out.valid && outputLast
  }

  io.out.bits.real := w_output.real >> shift_output
  io.out.bits.imag := w_output.imag >> shift_output

  // Private helpers

  private def selectedLookupAddress(i: Int): UInt = {
    val evenOff = if (evenNoOfStages) 1 else 0
    val (shiftedOffset, normalOffset) = if (isItDIF) (2, 1) else (evenOff, evenOff + 1)
    val shiftedAddress = w_twiddle_address((i << 1) + shiftedOffset)
    val normalAddress  = w_twiddle_address((i << 1) + normalOffset)
    if (params.runTime) Mux(isShiftedAddress, shiftedAddress, normalAddress) else normalAddress
  }

  private def clearStageTwiddleControl(i: Int): Unit = {
    w_invert_signals(i)  := false.B
    w_twiddle_address(i) := 0.U
    w_twiddles(i)        := 0.U.asTypeOf(w_twiddles(i))
  }

  private def stageActive(i: Int): Bool = if (params.runTime) runtimeStageActive.get(i) else true.B
  private def stageActiveOdd(i: Int): Bool =
    if (params.runTime) runtimeStageActive.get(i) && runtimeStageOdd.get(i) else staticStageOdd(i).B
  private def zeroAs[A <: Data](data: A): A = 0.U.asTypeOf(data)
  private def activeMux(i: Int, oddData: DspComplex[T], activeData: DspComplex[T], zeroType: DspComplex[T]): DspComplex[T] =
    Mux(stageActiveOdd(i), oddData, Mux(stageActive(i), activeData, zeroAs(zeroType))).asTypeOf(zeroType)
  private def mul(i: Int, in: DspComplex[T], tw: DspComplex[T], bpos: Int): Unit =
    w_mul_outputs(i) := Utils.complexMul(in, tw, bpos, params.numAddPipes, params.numMulPipes, params.trimType, params.use4Muls)

  private def wireStageTwiddleControl(stage: R22SDF[T], i: Int, delay: Int): Unit = {
    if (params.runTime) {
      when(stageHasTwiddleControl(i).B && runtimeStageActive.get(i)) {
        when(runtimeStageOdd.get(i)) { wireNonTrivialTwiddle(stage, i) }
          .otherwise      { wireTrivialInversion(stage, i, delay) }
      }.otherwise {
        clearStageTwiddleControl(i)
      }
    } else if (!stageHasTwiddleControl(i)) {
      clearStageTwiddleControl(i)
    } else if (staticStageOdd(i)) {
      wireNonTrivialTwiddle(stage, i)
    } else {
      wireTrivialInversion(stage, i, delay)
    }
  }

  private def wireNonTrivialTwiddle(stage: R22SDF[T], i: Int): Unit = {
    val counterWrap = stage.io.o_counter === ((1 << stage.io.o_counter.getWidth) - 1).U
    w_invert_signals(i) := false.B
    when(stage.io.i_en && counterWrap) {
      r_counters_msb(i) := ~r_counters_msb(i)
    }
    if (isItDIF) {
      w_twiddle_address(i) := Utils.difTwiddleAddress(
        i, stage.io.o_counter, r_counters_msb(i), noOfStages, params.runTime, isShiftedAddress)
      w_twiddles(i)        := ShiftRegister(lookupTwiddle(i), params.numAddPipes, true.B)
    } else {
      w_twiddle_address(i) := Utils.ditTwiddleAddress(
        i, stage.io.o_counter, r_counters_msb(i), noOfStages, params.runTime, isShiftedAddress)
      w_twiddles(i)        := lookupTwiddle(i)
    }
  }

  private def wireTrivialInversion(stage: R22SDF[T], i: Int, delay: Int): Unit = {
    w_invert_signals(i) := (if (isItDIF) {
      ShiftRegister(
        Mux(stage.io.o_counter < delay.U, Mux(stage.io.o_counter < (delay.U >> 1), false.B, true.B), false.B),
        params.numAddPipes, true.B)
    } else {
      Mux(stage.io.o_counter < delay.U, false.B, Mux(stage.io.o_counter < (delay * 3 / 2).U, false.B, true.B))
    })
    w_twiddle_address(i) := 0.U
    w_twiddles(i)        := 0.U.asTypeOf(w_twiddles(i))
  }

  /** Select the twiddle factor for stage i from the lookup table with correct index width. */
  private def lookupTwiddle(i: Int): DspComplex[T] = {
    if (noOfTwiddles == 1) {
      w_lookup_twiddles(0)
    } else if (noOfTwiddles > 1) {
      if (params.runTime && isItDIF) {
        val twIdx = log2Ceil(noOfTwiddles)
        val idx = Mux(isShiftedAddress,
          ((i >> 1).U - 1.U).asTypeOf(UInt(twIdx.W)),
          (i.U >> 1.U).asTypeOf(UInt(twIdx.W)))
        w_lookup_twiddles(idx)
      } else {
        w_lookup_twiddles(staticTwiddleLookupIndices(i))
      }
    } else {
      0.U.asTypeOf(params.twiddleType)
    }
  }

  private def bypassOrInverted(index: Int, data: DspComplex[T], inverted: DspComplex[T]): DspComplex[T] =
    if (params.runTime) Mux(!stageActiveOdd(index), ShiftRegister(inverted, complexMulLatency, true.B), data)
    else if (staticStageOdd(index)) data
    else ShiftRegister(inverted, complexMulLatency, true.B)

  /** Wire one DIF butterfly stage: route input, apply inversion/twiddle, return output. */
  private def connectDIFStage(stage: RadixIO[T], index: Int, prev_out: DspComplex[T], bpos: Int): DspComplex[T] = {
    // Stage input
    if (params.runTime) {
      when(runtimeStageActive.get(index)) {
        when(index.U === (noOfStages.U - activeStageCount)) { stage.in := io.in.bits }
          .otherwise                                        { stage.in := prev_out }
      }.otherwise { stage.in := 0.U.asTypeOf(stage.in) }
    } else if (index == 0) {
      stage.in := io.in.bits
    } else {
      stage.in := prev_out
    }

    val inverted = Utils.invertComplexData(stage.out, w_invert_signals(index))
    val out = Wire(stage.out.cloneType)

    if (index == 0) {                                      // first stage: no twiddle multiply
      w_mul_outputs(index) := Mux(stageActive(index), inverted, zeroAs(stage.in))
      out := ShiftRegister(w_mul_outputs(index), complexMulLatency, true.B)
    } else if (index == noOfStages - 1) {                  // last stage: pass through
      w_mul_outputs(index) := stage.out
      out := ShiftRegister(stage.out, complexMulLatency, true.B)
    } else if (index % 2 == 1) {                           // odd stage: non-trivial twiddle multiplier
      val mulIn = Wire(stage.in.cloneType)
      mulIn := activeMux(index, stage.out, w_stage_outputs(index + 1), stage.in)
      val mulTw = activeMux(index, w_twiddles(index), w_twiddles(index + 1), params.twiddleType)
      mul(index, mulIn, mulTw, bpos)
      out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    } else {                                               // even stage: forward previous multiplier result
      w_mul_outputs(index) := w_mul_outputs(index - 1).asTypeOf(w_mul_outputs(index))
      out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    }
    out
  }

  /** Wire one DIT butterfly stage: apply twiddle/pass, route to stage input, return output. */
  private def connectDITStage(stage: RadixIO[T], index: Int, prev_out: DspComplex[T], bpos: Int): DspComplex[T] = {
    val inverted    = Utils.invertComplexData(prev_out, w_invert_signals(index))
    val w_stage_out = Wire(stage.in.cloneType)
    stage.in := w_stage_out

    if (index == noOfStages - 1) {                         // last stage: trivial -j only
      w_mul_outputs(index) := Mux(stageActive(index), inverted, zeroAs(stage.in))
      w_stage_out := ShiftRegister(w_mul_outputs(index), complexMulLatency, true.B)
    } else if (index == 0) {                               // first stage: pass through
      w_mul_outputs(index) := prev_out
      w_stage_out := ShiftRegister(prev_out, complexMulLatency, true.B)
    } else if ((evenNoOfStages && index % 2 == 0) || (!evenNoOfStages && index % 2 == 1)) { // non-trivial twiddle
      val fbData = if (evenNoOfStages) w_stage_outputs(index - 2) else w_stage_outputs(index)
      val fbTw   = if (evenNoOfStages) w_twiddles(index - 1)      else w_twiddles(index + 1)
      val mulIn = Wire(stage.in.cloneType)
      mulIn := activeMux(index, prev_out.asTypeOf(stage.in), fbData, stage.in)
      val mulTw = activeMux(index, w_twiddles(index), fbTw, params.twiddleType)
      mul(index, mulIn, mulTw, bpos)
      w_stage_out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    } else {                                               // even stage: forward multiplier result
      w_mul_outputs(index) := (if (evenNoOfStages) w_mul_outputs(index + 1) else w_mul_outputs(index - 1))
      w_stage_out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    }
    w_stage_outputs(index)
  }
}
