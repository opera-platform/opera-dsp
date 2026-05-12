package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint

class R22FFT(val params: FFTParams) extends Module with HasIO {
  val io: FFTIO = IO(new FFTIO(params))

  // Constants
  private val isItDIF          = params.decimation == DIF
  private val noOfStages       = log2Ceil(params.fftSize)
  private val stageDelays      = (if (isItDIF) (0 until noOfStages).reverse else 0 until noOfStages).map(i => 1 << i)
  private val stageSizes       = stageDelays.map(_ << 1)
  private val cumulativeDelays = stageDelays.scanLeft(0)(_ + _)
  private val noOfTwiddles     = (noOfStages - 1) / 2
  private val stageCountWidth  = log2Ceil(noOfStages) + 1
  private val fftSizeCountType = UInt(log2Ceil(params.fftSize + 1).W)
  private val stageRoleIndices = (0 until noOfStages).map(i => if (isItDIF) i else noOfStages - i - 1)
  private val staticStageOdd   = stageRoleIndices.map(i => (i & 1) == 1)
  private val stageHasTwiddleControl = (0 until noOfStages).map(i => if (isItDIF) i != noOfStages - 1 else i != 0)

  require(params.fftSize >= 4, "R22FFT supports fftSize >= 4")
  require((noOfStages & 1) == 0, s"R22FFT supports only 4^N FFT sizes, got fftSize=${params.fftSize}")

  if (params.runTime) {
    (1 until noOfStages by 2).filter(_ + 1 < noOfStages).foreach { i =>
      require(
        params.resolvedTwiddleTrimTypes(i) == params.resolvedTwiddleTrimTypes(i + 1),
        s"R22FFT runtime mode shares a twiddle multiplier between stages $i and ${i + 1}; " +
          s"twiddleTrimTypes must match, got ${params.resolvedTwiddleTrimTypes(i)} and ${i + 1}: ${params.resolvedTwiddleTrimTypes(i + 1)}"
      )
    }
  }

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
  private val r_core_count = RegInit(0.U(log2Ceil(params.fftSize).W))
  private val r_initial_out_done = RegInit(false.B)
  private val r_out_count = RegInit(0.U(log2Ceil(params.fftSize).W))

  // Wires
  private val w_pair_mul_outputs =
    if (noOfTwiddles > 0) Some(Wire(MixedVec((0 until noOfTwiddles).map { pair =>
      if (isItDIF) params.stageOutputType((pair << 1) + 1) else params.stageInputType((pair << 1) + 2)
    }))) else None
  private val w_out_frame_busy = Wire(Bool())
  private val w_drain_done     = if (params.runTime) Some(Wire(Bool())) else None
  private val w_core_ready     = Wire(Bool())
  private val w_core_in_fire   = Wire(Bool())
  private val w_in_fire        = Wire(Bool())
  private val w_core_in        = Wire(params.inDataType)

  // Twiddle factor infrastructure
  private val LUT = if (noOfTwiddles > 0) Some(QuarterWaveSineLUT(1 << noOfStages, params.twiddleType)) else None
  private val w_pair_twiddles =
    if (noOfTwiddles > 0) Some(Wire(Vec(noOfTwiddles, params.twiddleType))) else None

  // Runtime-derived signals
  private val w_raw_cfg_load = io.i_load_cfg.getOrElse(false.B)
  private val w_runtime_drain_pending = r_cfg_drain_pending.getOrElse(false.B)
  private val w_runtime_apply_pending = r_apply_pending_cfg.getOrElse(false.B)
  private val w_apply_raw_cfg =
    if (!params.runTime) false.B
    else if (params.useBitReverse) w_raw_cfg_load
    else w_raw_cfg_load && !w_runtime_drain_pending && !w_out_frame_busy
  private val w_cfg_load = if (params.runTime) w_apply_raw_cfg || w_runtime_apply_pending else false.B
  private val w_cfg_reset = reset.asBool || w_cfg_load
  private val w_active_stage_count = r_num_stages.getOrElse(noOfStages.U(stageCountWidth.W))
  private val w_first_active_stage = noOfStages.U - w_active_stage_count
  private val w_active_fft_size = (if (params.runTime) 1.U << w_active_stage_count else params.fftSize.U).asTypeOf(fftSizeCountType)
  private val w_fft_or_ifft = r_fft_or_ifft.getOrElse(params.direction.B)

  if (params.runTime) {
    when(w_raw_cfg_load) {
      assert(io.i_size.get >= 2.U, "R22FFT runtime i_size must select at least 4 points")
      assert(!io.i_size.get(0), "R22FFT runtime i_size must be even, selecting only 4^N active FFT sizes")
    }
  }

  // Stage schedule
  private val w_zero_twiddle = 0.U.asTypeOf(params.twiddleType)
  private val w_active_cumulative_delay =
    if (params.runTime && isItDIF) VecInit(cumulativeDelays.map(_.U(log2Ceil(params.fftSize + 1).W)))(w_first_active_stage(log2Ceil(noOfStages + 1) - 1, 0))
    else 0.U(log2Ceil(params.fftSize + 1).W)

  private def cfgDelay[T <: Data](data: T, cycles: Int, init: T): T =
    withReset(w_cfg_reset) { ShiftRegister(data, cycles, init, true.B) }
  private def stageActive(i: Int): Bool = if (params.runTime) w_first_active_stage <= stageRoleIndices(i).U else true.B
  private def stageOdd(i: Int): Bool = if (params.runTime) (stageRoleIndices(i).U - w_first_active_stage)(0) else staticStageOdd(i).B
  private def stageActiveOdd(i: Int): Bool = if (params.runTime) stageActive(i) && stageOdd(i) else staticStageOdd(i).B
  private def finalStage[T <: Data](stageData: Seq[T]): T =
    if (params.runTime && !isItDIF) VecInit(stageData)((w_active_stage_count - 1.U)(log2Ceil(noOfStages) - 1, 0)) else stageData.last

  private case class StageSchedule(w_i_en: Bool, w_i_count: UInt, w_o_en: Bool, w_o_count: UInt)
  private val w_stage_schedule = (0 until noOfStages).foldLeft(Seq.empty[StageSchedule]) { (acc, i) =>
    val w_prev_en = if (i == 0) w_core_in_fire else acc.last.w_o_en
    val w_prev_count = if (i == 0) r_core_count else acc.last.w_o_count
    val w_load_core = if (params.runTime && isItDIF) i.U === w_first_active_stage else (i == 0).B
    val w_i_en = if (params.runTime) Mux(stageActive(i), Mux(w_load_core, w_core_in_fire, w_prev_en), false.B) else w_prev_en
    val w_i_count = if (params.runTime) Mux(stageActive(i), Mux(w_load_core, r_core_count, w_prev_count), 0.U) else w_prev_count
    acc :+ StageSchedule(w_i_en, w_i_count, cfgDelay(w_i_en, stageLatency, false.B), cfgDelay(w_i_count, stageLatency, 0.U.asTypeOf(w_i_count)))
  }

  private def bypassOrInverted(index: Int, data: DspComplex[FixedPoint], inverted: DspComplex[FixedPoint]): DspComplex[FixedPoint] = {
    val w_inverted_d = ShiftRegister(inverted, complexMulLatency)
    if (params.runTime) Mux(!stageActiveOdd(index), w_inverted_d, data)
    else if (staticStageOdd(index)) data
    else w_inverted_d
  }

  private def twiddleMul(
      index:     Int,
      data:      DspComplex[FixedPoint],
      twiddle:   DspComplex[FixedPoint],
      inputType: DspComplex[FixedPoint],
  ): DspComplex[FixedPoint] =
    Utils.complexMul(data, twiddle, inputType, params.numAddPipes, params.numMulPipes, params.resolvedTwiddleTrimTypes(index), params.use4Muls)

  private def twiddleAddress(index: Int): UInt = {
    val w_offset =
      if (isItDIF) cumulativeDelays(index + 1).U(log2Ceil(params.fftSize + 1).W) - w_active_cumulative_delay
      else cumulativeDelays(index).U(log2Ceil(params.fftSize + 1).W)
    (w_stage_schedule(index).w_i_count - w_offset).asTypeOf(UInt(noOfStages.W))
  }

  private def pairTwiddleForStage(index: Int): DspComplex[FixedPoint] =
    if (noOfTwiddles == 0) w_zero_twiddle
    else if (noOfTwiddles == 1) w_pair_twiddles.get(0)
    else {
      val pair = if (isItDIF) index >> 1 else (index - 1) >> 1
      if (pair >= 0 && pair < noOfTwiddles) w_pair_twiddles.get(pair) else w_zero_twiddle
    }

  private def trivialInvert(index: Int, stage: R22SDF): Bool = {
    val width = log2Ceil(stageSizes(index))
    val w_raw =
      if (isItDIF) {
        if (stageSizes(index) < 4) stage.io.o_counter < stageDelays(index).U
        else stage.io.o_counter(width - 1, width - 2) === 1.U
      } else {
        if (stageSizes(index) < 4) stage.io.o_counter >= (stageDelays(index) * 3 / 2).U
        else stage.io.o_counter(width - 1, width - 2).andR
      }
    if (isItDIF) cfgDelay(w_raw, params.numAddPipes, false.B) else w_raw
  }

  if (params.runTime) {
    when(r_apply_pending_cfg.get) {
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

    when(w_apply_raw_cfg || r_apply_pending_cfg.get) {
      r_num_stages.get := Mux(w_apply_raw_cfg, io.i_size.get, r_pending_num_stages.get)
      if (params.directionReg) r_fft_or_ifft.get := Mux(w_apply_raw_cfg, io.i_fft_or_ifft.get, r_pending_fft_or_ifft.get)
      if (params.divBy2Reg)    r_divBy2.get      := Mux(w_apply_raw_cfg, io.i_divBy2.get, r_pending_divBy2.get)
    }
  }

  if (noOfTwiddles > 0) {
    (0 until noOfTwiddles).foreach { pair =>
      val lookupIndex = if (isItDIF) pair else noOfTwiddles - 1 - pair
      val sourceStage = (lookupIndex << 1) + (if (isItDIF) 1 else 2)
      val stageN = 1 << (noOfStages - (pair << 1))
      w_pair_twiddles.get(lookupIndex) :=
        Radix22TwiddleFromLUT(twiddleAddress(sourceStage)(log2Ceil(stageN) - 1, 0), stageN, 1 << noOfStages, LUT.get)
    }
  }

  // Stage instantiation and twiddle control
  val sdf_stages: Seq[R22SDF] = stageDelays.zipWithIndex.map {
    case (delay, i) =>
      val stage = withReset(w_cfg_reset) { Module(new R22SDF(RadixParams(
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

      stage
  }

  private val w_stage_controls = sdf_stages.zipWithIndex.map { case (stage, i) =>
    val w_has_control = stageHasTwiddleControl(i).B && stageActive(i)
    val w_nontrivial_twiddle =
      if (params.runTime || stageHasTwiddleControl(i)) {
        val w_pair_twiddle = pairTwiddleForStage(i)
        if (isItDIF) ShiftRegister(w_pair_twiddle, params.numAddPipes) else w_pair_twiddle
      } else {
        w_zero_twiddle
      }
    val w_invert =
      if (params.runTime) Mux(w_has_control && !stageOdd(i), trivialInvert(i, stage), false.B)
      else if (stageHasTwiddleControl(i) && !staticStageOdd(i)) trivialInvert(i, stage) else false.B
    val w_twiddle =
      if (params.runTime) Mux(w_has_control && stageOdd(i), w_nontrivial_twiddle, w_zero_twiddle)
      else if (stageHasTwiddleControl(i) && staticStageOdd(i)) w_nontrivial_twiddle else w_zero_twiddle
    (w_invert, w_twiddle)
  }

  // Enable chain: valid and counter phase move through the active SDF stages.
  sdf_stages.zipWithIndex.foreach { case (s, index) =>
    s.io.i_en := w_stage_schedule(index).w_i_en
  }

  // Data path: connect stages in series, applying radix-2^2 trivial rotations and shared twiddles.
  val w_chain_outputs = sdf_stages.map(_.io).zipWithIndex.scanLeft(w_core_in: DspComplex[FixedPoint]) {
    case (prevOut, (stage, index)) =>
      val w_prev_stage_in = Utils.resizeComplex(prevOut, stage.in.cloneType)
      val w_core_stage_in = Utils.resizeComplex(w_core_in, stage.in.cloneType)
      val (w_invert, w_twiddle) = w_stage_controls(index)

      if (isItDIF) {
        val w_stage_in = Wire(stage.in.cloneType)
        if (params.runTime) {
          w_stage_in := Mux(stageActive(index), Mux(index.U === w_first_active_stage, w_core_stage_in, w_prev_stage_in), 0.U.asTypeOf(stage.in))
        } else if (index == 0) {
          w_stage_in := w_core_stage_in
        } else {
          w_stage_in := w_prev_stage_in
        }

        stage.in := w_stage_in
        val w_inverted = Utils.invertComplexData(stage.out, w_invert)
        val w_out = Wire(stage.out.cloneType)

        if (index == 0 || index == noOfStages - 1) {
          val w_pass_data = if (index == 0) Mux(stageActive(index), w_inverted, 0.U.asTypeOf(stage.out)) else stage.out
          w_out := ShiftRegister(w_pass_data, complexMulLatency)
        } else if (index % 2 == 1) {
          val pair = index >> 1
          val w_mul_input = Mux(
            stageActiveOdd(index),
            stage.out,
            Mux(stageActive(index), sdf_stages(index + 1).io.out.asTypeOf(stage.out), 0.U.asTypeOf(stage.out))
          )
          val w_mul_twiddle = Mux(
            stageActiveOdd(index),
            w_twiddle,
            Mux(stageActive(index), w_stage_controls(index + 1)._2, w_zero_twiddle)
          )
          w_pair_mul_outputs.get(pair) := twiddleMul(index, w_mul_input, w_mul_twiddle, params.stageOutputType(index))
          w_out := bypassOrInverted(index, w_pair_mul_outputs.get(pair).asTypeOf(stage.out), w_inverted)
        } else {
          w_out := bypassOrInverted(index, w_pair_mul_outputs.get((index - 1) >> 1).asTypeOf(stage.out), w_inverted)
        }
        w_out
      } else {
        val w_inverted = Utils.invertComplexData(w_prev_stage_in, w_invert)
        val w_stage_in = Wire(stage.in.cloneType)
        stage.in := w_stage_in

        if (index == noOfStages - 1 || index == 0) {
          val w_pass_data = if (index == noOfStages - 1) Mux(stageActive(index), w_inverted, 0.U.asTypeOf(stage.in)) else w_prev_stage_in
          w_stage_in := ShiftRegister(w_pass_data, complexMulLatency)
        } else if (index % 2 == 0) {
          val pair = (index - 2) >> 1
          val w_fb_data = sdf_stages(index - 2).io.out.asTypeOf(stage.in)
          val w_mul_input = Mux(stageActiveOdd(index), w_prev_stage_in, Mux(stageActive(index), w_fb_data, 0.U.asTypeOf(stage.in)))
          val w_mul_twiddle = Mux(stageActiveOdd(index), w_twiddle, Mux(stageActive(index), w_stage_controls(index - 1)._2, w_zero_twiddle))
          w_pair_mul_outputs.get(pair) := twiddleMul(index, w_mul_input, w_mul_twiddle, params.stageInputType(index))
          w_stage_in := bypassOrInverted(index, w_pair_mul_outputs.get(pair).asTypeOf(stage.in), w_inverted)
        } else {
          w_stage_in := bypassOrInverted(index, w_pair_mul_outputs.get((index - 1) >> 1).asTypeOf(stage.in), w_inverted)
        }

        stage.out
      }
  }.tail

  val w_final_stage_valid = finalStage(w_stage_schedule.map(_.w_o_en))
  val w_final_stage_count = finalStage(w_stage_schedule.map(_.w_o_count))
  val w_last_sample = (w_active_fft_size - 1.U).asTypeOf(r_out_count)
  val w_last_stage_frame_done = w_final_stage_valid && (w_final_stage_count === w_last_sample)
  val w_out_last = r_out_count === w_last_sample
  if (params.runTime) {
    w_drain_done.get := w_runtime_drain_pending && io.out.fire && w_out_last
  }

  val outQueue = withReset(w_cfg_reset) {
    Module(new Queue(UInt(params.fftOutputType.getWidth.W), entries = 2 * outputQueueReserve, pipe = true, flow = false))
  }
  outQueue.io.enq.bits := (if (isItDIF) {
    Utils.resizeComplex(ShiftRegister(sdf_stages.last.io.out, complexMulLatency), params.fftOutputType)
  } else {
    finalStage(w_chain_outputs.map(Utils.resizeComplex(_, params.fftOutputType)))
  }).asUInt
  outQueue.io.enq.valid := w_final_stage_valid && (r_initial_out_done || w_last_stage_frame_done) && !w_cfg_load
  outQueue.io.deq.ready := io.out.ready && !w_cfg_load

  val w_cfg_draining = w_runtime_drain_pending && !w_cfg_load
  w_core_ready := !w_cfg_load && outQueue.io.enq.ready && (outQueue.io.count < outputQueueReserve.U)
  w_in_fire := !w_runtime_drain_pending && io.in.valid && w_core_ready
  w_core_in_fire := (if (params.runTime) Mux(w_cfg_draining, w_core_ready, w_in_fire) else w_in_fire)
  w_core_in := (if (params.runTime) Mux(w_cfg_draining, 0.U.asTypeOf(params.inDataType), io.in.bits) else io.in.bits)
  io.in.ready  := !w_runtime_drain_pending && w_core_ready
  io.out.valid := outQueue.io.deq.valid && !w_cfg_load
  w_out_frame_busy := outQueue.io.deq.valid || r_out_count =/= 0.U

  Utils.assignFftOutputByDirection(outQueue.io.deq.bits.asTypeOf(params.fftOutputType), io.out.bits, w_fft_or_ifft)
  io.o_last := io.out.valid && w_out_last

  def updateFrameState(): Unit = {
    when(io.out.fire) {
      r_out_count := Mux(w_out_last, 0.U, r_out_count + 1.U)
    }
    when(w_core_in_fire) {
      r_core_count := Mux(r_core_count === w_last_sample, 0.U, r_core_count + 1.U)
    }
    r_initial_out_done := r_initial_out_done || w_last_stage_frame_done
  }

  if (params.runTime) {
    when(w_cfg_load) {
      Seq(r_out_count, r_core_count).foreach(_ := 0.U)
      r_initial_out_done := false.B
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
}
