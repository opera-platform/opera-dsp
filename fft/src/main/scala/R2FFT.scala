package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint

class R2FFT(val params: FFTParams) extends Module with HasIO {
  val io: FFTIO = IO(new FFTIO(params))

  // Constants
  private val isItDIF         = params.decimation == DIF
  private val noOfStages      = log2Ceil(params.fftSize)
  private val stageDelays     = (if (isItDIF) (0 until noOfStages).reverse else 0 until noOfStages).map(i => 1 << i)
  private val stageSizes      = stageDelays.map(_ << 1)
  private val stageCountWidth = log2Ceil(noOfStages) + 1
  private val fftSizeCountType = UInt(log2Ceil(params.fftSize + 1).W)
  private val stageRoleIndices = (0 until noOfStages).map(i => if (isItDIF) i else noOfStages - i - 1)
  private val stageHasTwiddle = (0 until noOfStages).map(i => if (isItDIF) i != noOfStages - 1 else i != 0)

  // Latencies
  private val complexMulLatency = if (params.use4Muls) params.numAddPipes + params.numMulPipes else 2 * params.numAddPipes + params.numMulPipes
  private val stageLatency      = params.numAddPipes + complexMulLatency
  private val latency           = stageLatency * noOfStages
  private val outputQueueReserve = latency + 1

  // Registers
  private val r_num_stages      = if (params.runTime) Some(RegInit(noOfStages.U(stageCountWidth.W))) else None
  private val r_fft_or_ifft     = if (params.directionReg) Some(RegInit(params.direction.B)) else None
  private val r_divBy2          = if (params.divBy2Reg) Some(RegInit(VecInit(params.stageDivBy2.map(_.B)))) else None
  private val r_pending_num_stages = if (params.runTime) Some(RegInit(noOfStages.U(stageCountWidth.W))) else None
  private val r_pending_fft_or_ifft = if (params.directionReg) Some(RegInit(params.direction.B)) else None
  private val r_pending_divBy2 = if (params.divBy2Reg) Some(RegInit(VecInit(params.stageDivBy2.map(_.B)))) else None
  private val r_cfg_drain_pending = if (params.runTime) Some(RegInit(false.B)) else None
  private val r_apply_pending_cfg = if (params.runTime) Some(RegInit(false.B)) else None
  private val r_static_ctrl_pending = if (!params.runTime && (params.directionReg || params.divBy2Reg)) Some(RegInit(false.B)) else None
  private val r_out_count = RegInit(0.U(log2Ceil(params.fftSize).W))

  // Wires
  private val w_output        = Wire(params.fftOutputType)
  private val w_stage_outputs = Wire(MixedVec((0 until noOfStages).map(i => params.stageOutputType(i))))
  private val w_chain_outputs = Wire(Vec(noOfStages, params.fftOutputType))
  private val w_mul_outputs   = Wire(MixedVec((0 until noOfStages).map(i => if (isItDIF) params.stageOutputType(i) else params.stageInputType(i))))
  private val w_twiddles      = Wire(Vec(noOfStages, params.twiddleType))
  private val w_twiddle_en    = Wire(Vec(noOfStages, Bool()))
  private val w_cfg_load       = Wire(Bool())
  private val w_out_frame_busy = Wire(Bool())
  private val w_drain_done     = if (params.runTime) Some(Wire(Bool())) else None
  private val w_core_ready     = Wire(Bool())
  private val w_core_in_fire   = Wire(Bool())
  private val w_in_fire        = Wire(Bool())
  private val w_core_in        = Wire(params.inDataType)

  // Twiddle factor infrastructure
  private val LUT = if (params.fftSize >= 4) Some(QuarterWaveSineLUT(1 << noOfStages, params.twiddleType)) else None

  // Runtime-derived signals
  private val w_raw_cfg_load       = io.i_load_cfg.getOrElse(false.B)
  private val w_runtime_drain_pending = r_cfg_drain_pending.getOrElse(false.B)
  private val w_runtime_apply_pending = r_apply_pending_cfg.getOrElse(false.B)
  private val w_apply_raw_cfg =
    if (!params.runTime) false.B
    else if (params.useBitReverse) w_raw_cfg_load
    else w_raw_cfg_load && !w_runtime_drain_pending && !w_out_frame_busy
  private val w_apply_pending_cfg = w_runtime_apply_pending
  w_cfg_load := (if (params.runTime) w_apply_raw_cfg || w_apply_pending_cfg else false.B)
  private val w_cfg_reset = reset.asBool || w_cfg_load
  private val w_active_stage_count = r_num_stages.getOrElse(noOfStages.U(stageCountWidth.W))
  private val w_first_active_stage = noOfStages.U - w_active_stage_count
  private val w_active_fft_size    = (if (params.runTime) 1.U << w_active_stage_count else params.fftSize.U).asTypeOf(fftSizeCountType)
  private val w_fft_or_ifft        = r_fft_or_ifft.getOrElse(params.direction.B)
  private val w_runtime_dif_inputs = if (params.runTime && isItDIF) {
    val w_delayed = Wire(Vec(noOfStages, params.inDataType))
    w_delayed(0) := w_core_in
    for (i <- 1 until noOfStages) {
      w_delayed(i) := withReset(w_cfg_reset) { ShiftRegister(w_delayed(i - 1), stageLatency, 0.U.asTypeOf(params.inDataType), true.B) }
    }
    Some(w_delayed)
  } else {
    None
  }

  // Stage helpers
  private def stageActive(i: Int): Bool = if (params.runTime) w_first_active_stage <= stageRoleIndices(i).U else true.B
  private def stageHasActiveTwiddle(i: Int): Bool = if (params.runTime) stageActive(i) && stageHasTwiddle(i).B else stageHasTwiddle(i).B
  private def stageTakesRuntimeInput(i: Int): Bool =
    if (params.runTime) {
      if (isItDIF) i.U === w_first_active_stage else (i == 0).B
    } else {
      (i == 0).B
    }

  // Twiddle helpers
  private val w_zero_twiddle = 0.U.asTypeOf(params.twiddleType)

  private def stageTwiddle(i: Int, counter: UInt): DspComplex[FixedPoint] = {
    if (stageSizes(i) >= 4) {
      val w_address = Wire(UInt(log2Ceil(stageSizes(i)).W))
      w_address := counter
      Radix2TwiddleFromLUT(w_address, stageSizes(i), 1 << noOfStages, LUT.get)
    } else {
      w_zero_twiddle
    }
  }

  private def delayedBypass(data: DspComplex[FixedPoint]): DspComplex[FixedPoint] =
    withReset(w_cfg_reset) { ShiftRegister(data, complexMulLatency, 0.U.asTypeOf(data), true.B) }

  private def asFftOutput(data: DspComplex[FixedPoint]): DspComplex[FixedPoint] = {
    val w_out = Wire(params.fftOutputType)
    w_out := data
    w_out
  }

  private def asStageInput(index: Int, data: DspComplex[FixedPoint]): DspComplex[FixedPoint] = {
    val w_out = Wire(params.stageInputType(index))
    w_out := data
    w_out
  }

  private def twiddleOrBypass(index: Int, data: DspComplex[FixedPoint], twiddle: DspComplex[FixedPoint], twiddleEn: Bool): DspComplex[FixedPoint] = {
    val w_mul = Wire(data.cloneType)
    val w_pass = Wire(data.cloneType)
    w_mul := Utils.complexMul(
      data,
      twiddle,
      params.stageInputType(index),
      params.numAddPipes,
      params.numMulPipes,
      params.resolvedTwiddleTrimTypes(index),
      params.use4Muls
    )
    w_pass := delayedBypass(data)
    Mux(withReset(w_cfg_reset) { ShiftRegister(twiddleEn, complexMulLatency, false.B, true.B) }, w_mul, w_pass)
  }

  if (params.runTime) {
    when(w_apply_pending_cfg) {
      r_cfg_drain_pending.get := false.B
      r_apply_pending_cfg.get := false.B
    }.elsewhen(w_raw_cfg_load && !w_apply_raw_cfg) {
      r_pending_num_stages.get := io.i_size.get
      if (params.directionReg) r_pending_fft_or_ifft.get := io.i_fft_or_ifft.get
      if (params.divBy2Reg)    r_pending_divBy2.get      := io.i_divBy2.get
      r_cfg_drain_pending.get := true.B
      r_apply_pending_cfg.get := false.B
    }.elsewhen(w_drain_done.get) {
      r_apply_pending_cfg.get := true.B
    }

    when(w_apply_raw_cfg) {
      r_num_stages.get := io.i_size.get
      if (params.directionReg) r_fft_or_ifft.get := io.i_fft_or_ifft.get
      if (params.divBy2Reg)    r_divBy2.get      := io.i_divBy2.get
    }.elsewhen(w_apply_pending_cfg) {
      r_num_stages.get := r_pending_num_stages.get
      if (params.directionReg) r_fft_or_ifft.get := r_pending_fft_or_ifft.get
      if (params.divBy2Reg)    r_divBy2.get      := r_pending_divBy2.get
    }
  }

  // Stage instantiation and twiddle control
  val sdf_stages: Seq[R2SDF] = stageDelays.zipWithIndex.map {
    case (delay, i) =>
      val stage = withReset(w_cfg_reset) { Module(new R2SDF(RadixParams(
        inDataType   = params.stageInputType(i),
        outDataType  = params.stageOutputType(i),
        twiddleType   = params.twiddleType,
        stageSize     = stageSizes(i),
        decimation    = params.decimation,
        overflowReg   = params.overflowReg,
        divBy2Reg     = params.divBy2Reg,
        divBy2        = params.stageDivBy2(i),
        growEnable    = params.stageGrowEnable(i),
        latency       = complexMulLatency,
        addPipeRegs   = params.numAddPipes,
        mulPipeRegs   = params.numMulPipes,
        dspMul4       = params.use4Muls,
        delay         = delay,
        bufferAsMem   = params.minSRAMdepth < delay,
        singlePortMem = params.singlePortSRAM,
        trimType      = params.resolvedStageTrimTypes(i),
      ))) }

      if (params.divBy2Reg)   stage.io.i_divBy2.get := r_divBy2.get(i)
      if (params.overflowReg) io.o_overflow.get(i)  := stage.io.o_overflow.get

      val w_low_counter = if (delay == 1) 0.U(1.W) else stage.io.o_counter(log2Ceil(delay) - 1, 0)
      if (isItDIF) {
        val w_raw_en = stageHasActiveTwiddle(i) && !stage.io.o_counter(log2Ceil(stageSizes(i)) - 1) && w_low_counter =/= 0.U
        w_twiddle_en(i) := withReset(w_cfg_reset) { ShiftRegister(w_raw_en, params.numAddPipes, false.B, true.B) }
        w_twiddles(i)   := withReset(w_cfg_reset) { ShiftRegister(stageTwiddle(i, w_low_counter), params.numAddPipes, w_zero_twiddle, true.B) }
      } else {
        val w_raw_en = stageHasActiveTwiddle(i) && stage.io.o_counter > delay.U
        w_twiddle_en(i) := w_raw_en
        w_twiddles(i)   := stageTwiddle(i, w_low_counter)
      }

      stage
  }

  // Enable chain: active stages forward valid data; inactive runtime stages only preserve schedule.
  sdf_stages.zipWithIndex.scanLeft(w_core_in_fire) { case (en, (s, index)) =>
    s.io.i_en := en
    val w_scheduled_out = withReset(w_cfg_reset) { ShiftRegister(en, stageLatency, false.B, true.B) }
    if (params.runTime) Mux(stageActive(index), s.io.o_en, w_scheduled_out) else s.io.o_en
  }

  // Data path: connect stages in series, applying twiddle factors as needed
  val w_first_stage_in = if (params.runTime) Mux(stageActive(0), w_core_in, 0.U.asTypeOf(params.inDataType)) else w_core_in
  sdf_stages.map(_.io).zipWithIndex.foldLeft(w_first_stage_in) {
    case (prevOut, (stage, index)) =>
      val w_active_in = if (params.runTime) {
        val w_runtime_in =
          if (isItDIF) asStageInput(index, w_runtime_dif_inputs.get.apply(index))
          else asStageInput(index, w_core_in)
        val w_selected_in = Mux(stageTakesRuntimeInput(index), w_runtime_in, asStageInput(index, prevOut))
        Mux(stageActive(index), w_selected_in, 0.U.asTypeOf(stage.in)).asTypeOf(stage.in)
      } else if (index == 0) {
        w_core_in
      } else {
        prevOut
      }

      if (isItDIF) {
        stage.in := asStageInput(index, w_active_in)
        w_stage_outputs(index) := stage.out
        w_mul_outputs(index) := Utils.complexMul(
          stage.out,
          w_twiddles(index),
          params.stageOutputType(index),
          params.numAddPipes,
          params.numMulPipes,
          params.resolvedTwiddleTrimTypes(index),
          params.use4Muls
        )
        w_chain_outputs(index) := Mux(
          withReset(w_cfg_reset) { ShiftRegister(w_twiddle_en(index), complexMulLatency, false.B, true.B) },
          w_mul_outputs(index),
          delayedBypass(stage.out)
        )
      } else {
        val w_stage_in = twiddleOrBypass(index, asStageInput(index, w_active_in), w_twiddles(index), w_twiddle_en(index))
        stage.in := w_stage_in
        w_stage_outputs(index) := stage.out
        w_mul_outputs(index)   := w_stage_in
        w_chain_outputs(index) := asFftOutput(stage.out)
      }

      w_chain_outputs(index)
  }

  val w_final_stage_index = (if (params.runTime && !isItDIF) w_active_stage_count - 1.U else (noOfStages - 1).U).asTypeOf(UInt(log2Ceil(noOfStages).W))
  val w_stage_tail = w_chain_outputs(w_final_stage_index)
  val w_final_stage_valid = VecInit(sdf_stages.map(_.io.o_en))(w_final_stage_index)
  val w_stage_out_valid = w_final_stage_valid && !w_cfg_load
  val w_last_sample = (w_active_fft_size - 1.U).asTypeOf(r_out_count)
  val w_out_last = r_out_count === w_last_sample
  if (params.runTime) {
    w_drain_done.get := w_runtime_drain_pending && io.out.fire && w_out_last
  }

  val outQueue = withReset(w_cfg_reset) {
    Module(new Queue(UInt(w_stage_tail.getWidth.W), entries = 2 * outputQueueReserve, pipe = true, flow = false))
  }
  outQueue.io.enq.bits  := w_stage_tail.asUInt
  outQueue.io.enq.valid := w_stage_out_valid
  outQueue.io.deq.ready := io.out.ready && !w_cfg_load

  val w_out_queue_has_reserve = outQueue.io.count < outputQueueReserve.U
  w_core_ready := !w_cfg_load && outQueue.io.enq.ready && w_out_queue_has_reserve
  w_in_fire := !w_runtime_drain_pending && io.in.valid && w_core_ready
  w_core_in_fire := (if (params.runTime) Mux(w_runtime_drain_pending && !w_cfg_load, w_core_ready, w_in_fire) else w_in_fire)
  w_core_in := (if (params.runTime) Mux(w_runtime_drain_pending && !w_cfg_load, 0.U.asTypeOf(params.inDataType), io.in.bits) else io.in.bits)
  io.in.ready  := !w_runtime_drain_pending && w_core_ready
  io.out.valid := outQueue.io.deq.valid && !w_cfg_load
  w_out_frame_busy := outQueue.io.deq.valid || r_out_count =/= 0.U

  Utils.assignFftOutputByDirection(outQueue.io.deq.bits.asTypeOf(w_stage_tail), w_output, w_fft_or_ifft)
  io.o_last := io.out.valid && w_out_last

  def updateFrameState(): Unit = {
    when(io.out.fire) {
      r_out_count := Mux(w_out_last, 0.U, r_out_count + 1.U)
    }
  }

  if (params.runTime) {
    when(w_cfg_load) {
      r_out_count := 0.U
    }.otherwise {
      updateFrameState()
    }
  } else {
    updateFrameState()
  }

  if (!params.runTime && (params.directionReg || params.divBy2Reg)) {
    val w_static_ctrl_apply = !w_out_frame_busy || (io.out.fire && w_out_last)

    when(w_raw_cfg_load) {
      if (params.directionReg) r_pending_fft_or_ifft.get := io.i_fft_or_ifft.get
      if (params.divBy2Reg)    r_pending_divBy2.get      := io.i_divBy2.get
      r_static_ctrl_pending.get := !w_static_ctrl_apply

      when(w_static_ctrl_apply) {
        if (params.directionReg) r_fft_or_ifft.get := io.i_fft_or_ifft.get
        if (params.divBy2Reg)    r_divBy2.get      := io.i_divBy2.get
      }
    }.elsewhen(r_static_ctrl_pending.get && w_static_ctrl_apply) {
      if (params.directionReg) r_fft_or_ifft.get := r_pending_fft_or_ifft.get
      if (params.divBy2Reg)    r_divBy2.get      := r_pending_divBy2.get
      r_static_ctrl_pending.get := false.B
    }
  }

  io.out.bits := w_output
}
