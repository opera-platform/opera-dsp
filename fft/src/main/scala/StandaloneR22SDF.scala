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
  private val r_counter_delay   = RegInit(0.U(noOfStages.W))
  private val r_pipeline_full   = RegInit(false.B)
  private val r_data_out_counter = RegInit(0.U(noOfStages.W))

  // Wires
  private val w_output           = Wire(params.outDataType)
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
  private val w_chirp_end      = (r_data_out_counter === (w_fft_size - 1.U)) && io.out.fire

  r_num_stages  := io.i_size.getOrElse(noOfStages.U)
  r_fft_or_ifft := io.i_fft_or_ifft.getOrElse(params.direction.B)

  if (params.runTime) w_fft_size := 1.U << r_num_stages
  else                w_fft_size := params.fftSize.U

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

      w_i_en_last_stage := stage.io.i_en && (i.U === r_num_stages - 1.U)

      // Twiddle address generation and inversion-signal logic
      val noTwining = if (isItDIF) (noOfStages - 1).U === i.U else i.U === 0.U
      when(!noTwining && isStageActive(i)) {
        when(isStageOdd(i)) {
          w_invert_signals(i) := false.B
          r_counters_msb(i)   := r_counters_msb(i) + (stage.io.i_en && stage.io.o_counter === ((1 << stage.io.o_counter.getWidth) - 1).U)
          if (isItDIF) {
            val addr = Cat(r_counters_msb(i), stage.io.o_counter) + (1 << (noOfStages - i - 1)).U
            w_twiddle_address(i) := Mux(isShiftedAddress, addr << 1, addr)
            w_twiddles(i)        := ShiftRegister(lookupTwiddle(i), params.numAddPipes, true.B)
          } else {
            val addr = Cat(r_counters_msb(i), stage.io.o_counter) + (1 << (i + 1)).U
            w_twiddle_address(i) := Mux(isShiftedAddress, addr << 1, addr)
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
  r_data_out_counter := r_data_out_counter + io.out.fire

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

  // Pipeline fill / output valid
  r_counter_delay := r_counter_delay + w_i_en_last_stage
  when(r_counter_delay === (w_fft_size - 1.U) && w_i_en_last_stage) { r_pipeline_full := true.B }

  if (isItDIF) w_stage_tail := ShiftRegister(w_stage_outputs.last, complexMulLatency, true.B)
  else         w_stage_tail := w_stage_outputs((r_num_stages - 1.U).asTypeOf(UInt(log2Ceil(noOfStages).W)))

  w_last_stage_valid := ShiftRegisterWithReset(
    r_pipeline_full && w_i_en_last_stage, outputLatency,
    resetData = false.B, reset = reset.asBool, en = true.B)

  io.in.ready := ~w_last_stage_valid
  private val shift_output = noOfStages.U * (params.expandLogic.sum != 0 || params.divBy2Reg).B

  if (latency == 0) {
    io.out.valid := w_last_stage_valid
    applyIfft(w_stage_tail, w_output)
  } else {
    val buf = Module(new Queue(chiselTypeOf(sdf_stages.last.io.out), entries = latency + 1, pipe = true, flow = true))
    buf.io.enq.bits  := w_stage_tail
    buf.io.enq.valid := w_last_stage_valid
    buf.io.deq.ready := io.out.ready
    io.in.ready  := !r_pipeline_full || buf.io.enq.ready
    io.out.valid := buf.io.deq.valid
    applyIfft(buf.io.deq.bits, w_output)
  }

  io.out.bits.real := w_output.real >> shift_output
  io.out.bits.imag := w_output.imag >> shift_output
  io.o_last        := w_chirp_end

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
        w_mul_outputs(index) := 0.U.asTypeOf(stage.in)
      }
      out := ShiftRegister(w_mul_outputs(index), complexMulLatency, true.B)
    } else if (index == noOfStages - 1) {                  // last stage: pass through
      w_mul_outputs(index) := stage.out
      out := ShiftRegister(stage.out, complexMulLatency, true.B)
    } else if (index % 2 == 1) {                           // odd stage: non-trivial twiddle multiplier
      val mulIn = Mux(isActiveOdd(index), stage.out,
        Mux(isStageActive(index), w_stage_outputs(index + 1), 0.U.asTypeOf(stage.in))
      ).asTypeOf(params.inDataType)
      val mulTw = Mux(isActiveOdd(index), w_twiddles(index),
        Mux(isStageActive(index), w_twiddles(index + 1), 0.U.asTypeOf(params.twiddleType))
      ).asTypeOf(params.twiddleType)
      w_mul_outputs(index) := complexMul(mulIn, mulTw, bpos)
      out := Mux(!isActiveOdd(index), ShiftRegister(inverted, complexMulLatency, true.B), w_mul_outputs(index))
    } else {                                               // even stage: forward previous multiplier result
      w_mul_outputs(index) := w_mul_outputs(index - 1).asTypeOf(w_mul_outputs(index))
      out := Mux(!isActiveOdd(index), ShiftRegister(inverted, complexMulLatency, true.B), w_mul_outputs(index))
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
        w_mul_outputs(index) := 0.U.asTypeOf(stage.in)
      }
      w_stage_out := ShiftRegister(w_mul_outputs(index), complexMulLatency, true.B)
    } else if (index == 0) {                               // first stage: pass through
      w_mul_outputs(index) := prev_out
      w_stage_out := ShiftRegister(prev_out, complexMulLatency, true.B)
    } else if ((evenNoOfStages && index % 2 == 0) || (!evenNoOfStages && index % 2 == 1)) { // non-trivial twiddle
      val fbData = if (evenNoOfStages) w_stage_outputs(index - 2) else w_stage_outputs(index)
      val fbTw   = if (evenNoOfStages) w_twiddles(index - 1)      else w_twiddles(index + 1)
      val mulIn = Mux(isActiveOdd(index), prev_out.asTypeOf(stage.in),
        Mux(isStageActive(index), fbData, 0.U.asTypeOf(stage.in))
      ).asTypeOf(params.inDataType)
      val mulTw = Mux(isActiveOdd(index), w_twiddles(index),
        Mux(isStageActive(index), fbTw, 0.U.asTypeOf(params.twiddleType))
      ).asTypeOf(params.twiddleType)
      w_mul_outputs(index) := complexMul(mulIn, mulTw, bpos)
      w_stage_out := Mux(!isActiveOdd(index), ShiftRegister(inverted, complexMulLatency, true.B), w_mul_outputs(index))
    } else {                                               // even stage: forward multiplier result
      w_mul_outputs(index) := (if (evenNoOfStages) w_mul_outputs(index + 1) else w_mul_outputs(index - 1))
      w_stage_out := Mux(!isActiveOdd(index), ShiftRegister(inverted, complexMulLatency, true.B), w_mul_outputs(index))
    }
    w_stage_outputs(index)
  }
}
