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

  // Latencies
  private val complexMulLatency = if (params.use4Muls) params.numAddPipes + params.numMulPipes
                                  else 2 * params.numAddPipes + params.numMulPipes
  private val outputLatency     = params.numAddPipes + complexMulLatency
  private val latency           = outputLatency * noOfStages

  // Registers
  private val r_num_stages      = RegInit(noOfStages.U((log2Ceil(noOfStages) + 1).W))
  private val r_fft_or_ifft     = RegInit(true.B)
  private val r_counters_msb    = RegInit(VecInit(Seq.fill(noOfStages)(false.B)))
  private val inputSampleCount  = RegInit(0.U(log2Ceil(params.fftSize).W))
  private val acceptedFrameCount = RegInit(0.U(1.W))
  private val lastStageSampleCount = RegInit(0.U(log2Ceil(params.fftSize).W))

  // Wires
  private val w_output           = Wire(params.outDataType)
  private val w_stage_en         = Wire(Vec(noOfStages, Bool()))
  private val w_last_stage_index = Wire(UInt(log2Ceil(noOfStages).W))
  private val w_i_en_last_stage  = Wire(Bool())
  private val w_last_stage_valid = Wire(Bool())
  private val w_fft_size         = Wire(UInt(log2Ceil(params.fftSize + 1).W))
  private val w_stage_outputs    = Wire(Vec(noOfStages, params.outDataType))
  private val w_stage_tail       = Wire(params.outDataType)
  private val w_mul_outputs      = Wire(MixedVec((0 until noOfStages).map { i => params.protoIQstages(i) }))
  private val w_invert_signals   = Wire(Vec(noOfStages, Bool()))

  // Stage-activity flags
  private val isStageOdd    = Wire(Vec(noOfStages, Bool()))
  private val isStageActive = Wire(Vec(noOfStages, Bool()))
  private val isActiveOdd   = Wire(Vec(noOfStages, Bool()))

  // Twiddle factor infrastructure
  private val w_lookup_twiddles = Wire(Vec(noOfTwiddles, params.twiddleType))
  private val w_lookup_address  = Wire(Vec(noOfTwiddles, UInt(noOfStages.W)))
  private val w_twiddles        = Wire(Vec(noOfStages, params.twiddleType))
  private val w_twiddle_address = Wire(Vec(noOfStages, UInt(noOfStages.W)))
  private val LUT               = QuarterWaveSineLUT[T](1 << noOfStages, params.twiddleType)

  // Runtime-derived signals
  private val isShiftedAddress = r_num_stages(0) ^ noOfStages.U.extract(0)

  if (params.runTime) w_fft_size := 1.U << r_num_stages
  else                w_fft_size := params.fftSize.U

  val inputFrameStart = io.in.fire && inputSampleCount === 0.U
  val inputFrameEnd   = io.in.fire && inputSampleCount === (w_fft_size - 1.U)
  val sdfWarm         = acceptedFrameCount =/= 0.U

  when(inputFrameStart) {
    r_num_stages  := io.i_size.getOrElse(noOfStages.U)
    r_fft_or_ifft := io.i_fft_or_ifft.getOrElse(params.direction.B)
  }

  when(inputFrameEnd) {
    inputSampleCount := 0.U
    acceptedFrameCount := 1.U
  }.elsewhen(io.in.fire) {
    inputSampleCount := inputSampleCount + 1.U
  }

  w_last_stage_index :=
    (if (isItDIF) (noOfStages - 1).U else (r_num_stages - 1.U)).asTypeOf(UInt(log2Ceil(noOfStages).W))
  w_i_en_last_stage := w_stage_en(w_last_stage_index)

  // Stage-activity computation
  isStageActive.zip(isStageOdd).zipWithIndex.foreach {
    case ((active, odd), i) =>
      val index = if (isItDIF) i else noOfStages - i - 1
      active := noOfStages.U - r_num_stages <= index.U
      odd    := (index.U - (noOfStages.U - r_num_stages))(0)
  }
  isActiveOdd.zipWithIndex.foreach { case (ao, i) => ao := isStageActive(i) && isStageOdd(i) }

  // Twiddle LUT wiring
  w_lookup_address.zipWithIndex.foreach {
    case (address, i) =>
      val evenOff = if (evenNoOfStages) 1 else 0
      val (off0, off1) = if (isItDIF) (2, 1) else (evenOff, evenOff + 1)
      address := Mux(isShiftedAddress,
        w_twiddle_address((i << 1) + off0),
        w_twiddle_address((i << 1) + off1))
  }
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
      val noTwining = if (isItDIF) (noOfStages - 1).U === i.U else i.U === 0.U
      when(!noTwining && isStageActive(i)) {
        when(isStageOdd(i)) {
          w_invert_signals(i) := false.B
          r_counters_msb(i)   := r_counters_msb(i) + (stage.io.i_en && stage.io.o_counter === ((1 << stage.io.o_counter.getWidth) - 1).U)
          if (isItDIF) {
            w_twiddle_address(i) := difTwiddleAddress(i, stage.io.o_counter)
            w_twiddles(i)        := ShiftRegister(lookupTwiddle(i), params.numAddPipes, true.B)
          } else {
            w_twiddle_address(i) := ditTwiddleAddress(i, stage.io.o_counter)
            w_twiddles(i)        := lookupTwiddle(i)
          }
        }.otherwise { // trivial: multiply by -j over mid-range samples
          if (isItDIF)
            w_invert_signals(i) := ShiftRegister(
              Mux(stage.io.o_counter < delay.U, Mux(stage.io.o_counter < (delay.U >> 1), false.B, true.B), false.B),
              params.numAddPipes, true.B)
          else
            w_invert_signals(i) := Mux(stage.io.o_counter < delay.U, false.B,
              Mux(stage.io.o_counter < (delay * 3 / 2).U, false.B, true.B))
          w_twiddle_address(i) := 0.U
          w_twiddles(i)        := 0.U.asTypeOf(w_twiddles(i))
        }
      }.otherwise {
        w_invert_signals(i)  := false.B
        w_twiddle_address(i) := 0.U
        w_twiddles(i)        := 0.U.asTypeOf(w_twiddles(i))
      }
      stage
  }

  // Enable chain
  private val w_en: Bool = io.in.fire
  sdf_stages.scanLeft(w_en) { case (en, s) => s.io.i_en := en; s.io.o_en }

  // Data path
  sdf_stages.map(_.io).zipWithIndex.foldLeft(Mux(isStageActive(0), io.in.bits, 0.U.asTypeOf(params.inDataType))) {
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
  else         w_stage_tail := w_stage_outputs((r_num_stages - 1.U).asTypeOf(UInt(log2Ceil(noOfStages).W)))

  w_last_stage_valid := ShiftRegisterWithReset(
    sdfWarm && w_i_en_last_stage, outputLatency,
    resetData = false.B, reset = reset.asBool, en = true.B)

  io.in.ready := ~w_last_stage_valid
  private val shift_output = noOfStages.U * (params.expandLogic.sum != 0 || params.divBy2Reg).B
  val lastStageLast = lastStageSampleCount === (w_fft_size - 1.U)

  if (latency == 0) {
    io.out.valid := w_last_stage_valid
    applyIfft(w_stage_tail, w_output)
    when(io.out.fire) {
      lastStageSampleCount := Mux(lastStageLast, 0.U, lastStageSampleCount + 1.U)
    }
    io.o_last := io.out.valid && lastStageLast
  } else {
    val outQueue = Module(new Queue(new Bundle {
      val data = chiselTypeOf(w_stage_tail)
      val last = Bool()
    }, entries = latency + 1, pipe = true, flow = true))
    outQueue.io.enq.bits.data := w_stage_tail
    outQueue.io.enq.bits.last := lastStageLast
    outQueue.io.enq.valid := w_last_stage_valid
    outQueue.io.deq.ready := io.out.ready

    when(outQueue.io.enq.fire) {
      lastStageSampleCount := Mux(lastStageLast, 0.U, lastStageSampleCount + 1.U)
    }

    io.in.ready  := !sdfWarm || outQueue.io.enq.ready
    io.out.valid := outQueue.io.deq.valid
    applyIfft(outQueue.io.deq.bits.data, w_output)
    io.o_last := outQueue.io.deq.valid && outQueue.io.deq.bits.last
  }

  io.out.bits.real := w_output.real >> shift_output
  io.out.bits.imag := w_output.imag >> shift_output

  // Private helpers

  /** Select the twiddle factor for stage i from the lookup table with correct index width. */
  private def lookupTwiddle(i: Int): DspComplex[T] = {
    if (noOfTwiddles == 1) {
      w_lookup_twiddles(0)
    } else if (noOfTwiddles > 1) {
      val twIdx = log2Ceil(noOfTwiddles)
      val idx: UInt =
        if (isItDIF)
          Mux(isShiftedAddress,
            ((i >> 1).U - 1.U).asTypeOf(UInt(twIdx.W)),
            (i.U >> 1.U).asTypeOf(UInt(twIdx.W)))
        else
          (if (evenNoOfStages) ((i.U - 1.U) >> 1.U) else (i.U >> 1.U)).asTypeOf(UInt(twIdx.W))
      w_lookup_twiddles(idx)
    } else {
      0.U.asTypeOf(params.twiddleType)
    }
  }

  private def fitTwiddleAddress(address: UInt): UInt = {
    address.asTypeOf(UInt(noOfStages.W))
  }

  private def difTwiddleAddress(stageIndex: Int, counter: UInt): UInt = {
    val baseAddress = Cat(r_counters_msb(stageIndex), counter) + (1 << (noOfStages - stageIndex - 1)).U
    fitTwiddleAddress(Mux(isShiftedAddress, baseAddress << 1, baseAddress))
  }

  private def ditTwiddleAddress(stageIndex: Int, counter: UInt): UInt = {
    val baseAddress = Cat(r_counters_msb(stageIndex), counter) + (1 << (stageIndex + 1)).U
    fitTwiddleAddress(Mux(isShiftedAddress, baseAddress << 1, baseAddress))
  }

  /** Pipelined complex multiply with the standard FFT DSP context. */
  private def complexMul(input: DspComplex[T], twiddle: DspComplex[T], bpos: Int): DspComplex[T] =
    DspContext.alter(DspContext.current.copy(
      numAddPipes     = params.numAddPipes,
      numMulPipes     = params.numMulPipes,
      trimType        = params.trimType,
      overflowType    = Grow,
      complexUse4Muls = params.use4Muls
    )) { input.context_*(twiddle).trimBinary(bpos) }

  /** Conditionally rotate data by -j (swap real/imag and negate new imag). */
  private def applyInversion(data: DspComplex[T], invertSig: Bool): DspComplex[T] = {
    val out = Wire(data.cloneType)
    out.real := Mux(invertSig,  data.imag, data.real)
    out.imag := Mux(invertSig, -data.real, data.imag)
    out
  }

  /** Drive dst from src, swapping real/imag when in IFFT mode. */
  private def applyIfft(src: DspComplex[T], dst: DspComplex[T]): Unit =
    when(r_fft_or_ifft) { dst := src }
    .otherwise          { dst.real := src.imag; dst.imag := src.real }

  private def delayed(data: DspComplex[T]): DspComplex[T] =
    ShiftRegister(data, complexMulLatency, true.B)

  private def zeroLike(data: DspComplex[T]): DspComplex[T] =
    0.U.asTypeOf(data)

  private def bypassOrInverted(index: Int, data: DspComplex[T], inverted: DspComplex[T]): DspComplex[T] =
    Mux(!isActiveOdd(index), delayed(inverted), data)

  /** Wire one DIF butterfly stage: route input, apply inversion/twiddle, return output. */
  private def connectDIFStage(stage: RadixIO[T], index: Int, prev_out: DspComplex[T], bpos: Int): DspComplex[T] = {
    // Stage input
    when(isStageActive(index)) {
      when(index.U === (noOfStages.U - r_num_stages)) { stage.in := io.in.bits }
      .otherwise                                       { stage.in := prev_out }
    }.otherwise { stage.in := 0.U.asTypeOf(stage.in) }

    val inverted = applyInversion(stage.out, w_invert_signals(index))
    val out = Wire(stage.out.cloneType)

    if (index == 0) {                                      // first stage: no twiddle multiply
      when(isStageActive(index)) {
        w_mul_outputs(index) := inverted
      }.otherwise {
        w_mul_outputs(index) := zeroLike(stage.in)
      }
      out := delayed(w_mul_outputs(index))
    } else if (index == noOfStages - 1) {                  // last stage: pass through
      w_mul_outputs(index) := stage.out
      out := delayed(stage.out)
    } else if (index % 2 == 1) {                           // odd stage: non-trivial twiddle multiplier
      val mulIn = Wire(stage.in.cloneType)
      mulIn := Mux(isActiveOdd(index), stage.out,
        Mux(isStageActive(index), w_stage_outputs(index + 1), zeroLike(stage.in))
      ).asTypeOf(stage.in)
      val mulTw = Mux(isActiveOdd(index), w_twiddles(index),
        Mux(isStageActive(index), w_twiddles(index + 1), 0.U.asTypeOf(params.twiddleType))
      ).asTypeOf(params.twiddleType)
      w_mul_outputs(index) := complexMul(mulIn, mulTw, bpos)
      out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    } else {                                               // even stage: forward previous multiplier result
      w_mul_outputs(index) := w_mul_outputs(index - 1).asTypeOf(w_mul_outputs(index))
      out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    }
    out
  }

  /** Wire one DIT butterfly stage: apply twiddle/pass, route to stage input, return output. */
  private def connectDITStage(stage: RadixIO[T], index: Int, prev_out: DspComplex[T], bpos: Int): DspComplex[T] = {
    val inverted    = applyInversion(prev_out, w_invert_signals(index))
    val w_stage_out = Wire(stage.in.cloneType)
    stage.in := w_stage_out

    if (index == noOfStages - 1) {                         // last stage: trivial -j only
      when(isStageActive(index)) {
        w_mul_outputs(index) := inverted
      }.otherwise {
        w_mul_outputs(index) := zeroLike(stage.in)
      }
      w_stage_out := delayed(w_mul_outputs(index))
    } else if (index == 0) {                               // first stage: pass through
      w_mul_outputs(index) := prev_out
      w_stage_out := delayed(prev_out)
    } else if ((evenNoOfStages && index % 2 == 0) || (!evenNoOfStages && index % 2 == 1)) { // non-trivial twiddle
      val fbData = if (evenNoOfStages) w_stage_outputs(index - 2) else w_stage_outputs(index)
      val fbTw   = if (evenNoOfStages) w_twiddles(index - 1)      else w_twiddles(index + 1)
      val mulIn = Wire(stage.in.cloneType)
      mulIn := Mux(isActiveOdd(index), prev_out.asTypeOf(stage.in),
        Mux(isStageActive(index), fbData, zeroLike(stage.in))
      ).asTypeOf(stage.in)
      val mulTw = Mux(isActiveOdd(index), w_twiddles(index),
        Mux(isStageActive(index), fbTw, 0.U.asTypeOf(params.twiddleType))
      ).asTypeOf(params.twiddleType)
      w_mul_outputs(index) := complexMul(mulIn, mulTw, bpos)
      w_stage_out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    } else {                                               // even stage: forward multiplier result
      w_mul_outputs(index) := (if (evenNoOfStages) w_mul_outputs(index + 1) else w_mul_outputs(index - 1))
      w_stage_out := bypassOrInverted(index, w_mul_outputs(index), inverted)
    }
    w_stage_outputs(index)
  }
}
