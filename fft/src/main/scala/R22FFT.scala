package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

// TODO: For output scaling maybe do div2 instead of truncation. More logic but more accurate
// TODO: Refactor the names of signals. Use either name_second_name or nameSecondName consistently, and make sure the names are descriptive enough to understand their purpose without needing comments.
class R22FFT[T <: Data: Real: BinaryRepresentation](val params: FFTParams[T]) extends Module with HasIO[T] {
  val io: FFTIO[T] = IO(new FFTIO(params))

  // Constants
  private val isItDIF        = params.decimation == DIF
  private val noOfStages     = log2Ceil(params.fftSize)
  private val evenNoOfStages = noOfStages % 2 == 0
  private val stageDelays    = (if (isItDIF) (0 until noOfStages).reverse else 0 until noOfStages).map(i => 1 << i)
  private val noOfTwiddles   = (noOfStages - 1) / 2
  private val stageCountWidth = log2Ceil(noOfStages) + 1
  private val stageRoleIndices = (0 until noOfStages).map(i => if (isItDIF) i else noOfStages - i - 1)
  private val staticStageOdd = stageRoleIndices.map(i => (i & 1) == 1)
  private val stageHasTwiddleControl = (0 until noOfStages).map(i => if (isItDIF) i != noOfStages - 1 else i != 0)

  if (params.runTime) {
    (1 until noOfStages by 2).filter(_ + 1 < noOfStages).foreach { i =>
      require(
        params.resolvedTwiddleTrimTypes(i) == params.resolvedTwiddleTrimTypes(i + 1),
        s"R22FFT runtime mode shares a twiddle multiplier between stages $i and ${i + 1}; " +
          s"twiddleTrimTypes must match, got ${params.resolvedTwiddleTrimTypes(i)} and ${params.resolvedTwiddleTrimTypes(i + 1)}"
      )
    }
  }

  // Latencies
  private val complexMulLatency = if (params.use4Muls) params.numAddPipes + params.numMulPipes else 2 * params.numAddPipes + params.numMulPipes
  private val latency           = (params.numAddPipes + complexMulLatency) * noOfStages
  private val outputQueueReserve = latency + 1

  // Registers
  private val r_num_stages      = if (params.runTime) Some(RegInit(noOfStages.U(stageCountWidth.W))) else None
  private val r_fft_or_ifft     = if (params.directionReg) Some(RegInit(params.direction.B)) else None
  private val r_divBy2          = if (params.divBy2Reg) Some(RegInit(VecInit(params.divBy2.toIndexedSeq.map(_.B)))) else None
  private val r_counters_msb    = RegInit(VecInit(Seq.fill(noOfStages)(false.B)))
  private val outputSampleCount = RegInit(0.U(log2Ceil(params.fftSize).W))

  // Wires
  private val w_output         = Wire(params.outDataType)
  private val w_stage_outputs  = Wire(Vec(noOfStages, params.outDataType))
  private val w_mul_outputs    = Wire(MixedVec((0 until noOfStages).map(i => params.protoIQstages(i))))
  private val w_invert_signals = Wire(Vec(noOfStages, Bool()))

  // Twiddle factor infrastructure
  private val w_lookup_twiddles = Wire(Vec(noOfTwiddles, params.twiddleType))
  private val w_lookup_address  = Wire(Vec(noOfTwiddles, UInt(noOfStages.W)))
  private val w_twiddles        = Wire(Vec(noOfStages, params.twiddleType))
  private val w_twiddle_address = Wire(Vec(noOfStages, UInt(noOfStages.W)))
  private val LUT               = QuarterWaveSineLUT[T](1 << noOfStages, params.twiddleType)

  // Runtime-derived signals
  private val cfgLoad          = io.i_load_cfg.getOrElse(false.B)
  private val cfgReset         = reset.asBool || cfgLoad
  private val activeStageCount = r_num_stages.getOrElse(noOfStages.U(stageCountWidth.W))
  private val firstActiveStage = noOfStages.U - activeStageCount
  private val activeFftSize    = if (params.runTime) 1.U << activeStageCount else params.fftSize.U
  private val isShiftedAddress = if (params.runTime) activeStageCount(0) ^ (noOfStages & 1).B else false.B
  private val fftOrIfft        = r_fft_or_ifft.getOrElse(params.direction.B)
  private val outputShift      = if (params.expandLogic.sum != 0 || params.divBy2Reg) noOfStages.U else 0.U

  // Stage helpers
  private def stageOdd(i: Int): Bool       = if (params.runTime) (stageRoleIndices(i).U - firstActiveStage)(0) else staticStageOdd(i).B
  private def stageActive(i: Int): Bool    = if (params.runTime) firstActiveStage <= stageRoleIndices(i).U else true.B
  private def stageActiveOdd(i: Int): Bool = if (params.runTime) stageActive(i) && stageOdd(i) else staticStageOdd(i).B

  // Datapath helpers
  private def bypassOrInverted(index: Int, data: DspComplex[T], inverted: DspComplex[T]): DspComplex[T] =
    if (params.runTime) Mux(!stageActiveOdd(index), ShiftRegister(inverted, complexMulLatency, true.B), data)
    else if (staticStageOdd(index)) data
    else ShiftRegister(inverted, complexMulLatency, true.B)

  // Twiddle control helpers
  private def clearStageTwiddleControl(i: Int): Unit = {
    w_invert_signals(i)  := false.B
    w_twiddle_address(i) := 0.U
    w_twiddles(i)        := 0.U.asTypeOf(w_twiddles(i))
  }

  private def wireNonTrivialTwiddle(stage: R22SDF[T], i: Int): Unit = {
    val counterWrap = stage.io.o_counter === ((1 << stage.io.o_counter.getWidth) - 1).U
    w_invert_signals(i) := false.B
    when(!cfgLoad && stage.io.i_en && counterWrap) {
      r_counters_msb(i) := ~r_counters_msb(i)
    }
    w_twiddle_address(i) := (if (isItDIF) {
      Utils.difTwiddleAddress(i, stage.io.o_counter, r_counters_msb(i), noOfStages, params.runTime, isShiftedAddress)
    } else {
      Utils.ditTwiddleAddress(i, stage.io.o_counter, r_counters_msb(i), noOfStages, params.runTime, isShiftedAddress)
    })

    val twiddle =
      if (noOfTwiddles == 0) 0.U.asTypeOf(params.twiddleType)
      else if (noOfTwiddles == 1) w_lookup_twiddles(0)
      else if (params.runTime && isItDIF) {
        val twIdx = log2Ceil(noOfTwiddles)
        val idx = Mux(isShiftedAddress,
          ((i >> 1).U - 1.U).asTypeOf(UInt(twIdx.W)),
          (i.U >> 1.U).asTypeOf(UInt(twIdx.W)))
        w_lookup_twiddles(idx)
      } else {
        val idx = if (isItDIF || !evenNoOfStages) i >> 1 else (i - 1) >> 1
        if (idx >= 0 && idx < noOfTwiddles) w_lookup_twiddles(idx) else 0.U.asTypeOf(params.twiddleType)
      }
    w_twiddles(i) := (if (isItDIF) ShiftRegister(twiddle, params.numAddPipes, true.B) else twiddle)
  }

  private def wireTrivialInversion(stage: R22SDF[T], i: Int, delay: Int): Unit = {
    val invert = if (isItDIF) stage.io.o_counter >= (delay.U >> 1) && stage.io.o_counter < delay.U else stage.io.o_counter >= (delay * 3 / 2).U
    w_invert_signals(i) := (if (isItDIF) ShiftRegister(invert, params.numAddPipes, true.B) else invert)
    w_twiddle_address(i) := 0.U
    w_twiddles(i)        := 0.U.asTypeOf(w_twiddles(i))
  }

  when(cfgLoad) {
    if (params.runTime)      r_num_stages.get  := io.i_size.get
    if (params.directionReg) r_fft_or_ifft.get := io.i_fft_or_ifft.get
    if (params.divBy2Reg)    r_divBy2.get      := io.i_divBy2.get
    r_counters_msb := VecInit(Seq.fill(noOfStages)(false.B))
  }

  // Twiddle LUT wiring
  w_lookup_address.zipWithIndex.foreach {
    case (address, i) =>
      val evenOff = if (evenNoOfStages) 1 else 0
      val (shiftedOffset, normalOffset) = if (isItDIF) (2, 1) else (evenOff, evenOff + 1)
      address := (if (params.runTime)
        Mux(isShiftedAddress, w_twiddle_address((i << 1) + shiftedOffset), w_twiddle_address((i << 1) + normalOffset))
      else w_twiddle_address((i << 1) + normalOffset))
  }
  (0 until noOfStages by 2).dropRight(1).zipWithIndex.foreach {
    case (m, i) =>
      val stageN = 1 << (noOfStages - m)
      w_lookup_twiddles(if (isItDIF) i else noOfTwiddles - 1 - i) :=
        Radix22TwiddleFromLUT[T](w_lookup_address(if (isItDIF) i else noOfTwiddles - 1 - i)(log2Ceil(stageN) - 1, 0), stageN, 1 << noOfStages, LUT)
  }

  // Stage instantiation and twiddle address generation
  val sdf_stages: Seq[R22SDF[T]] = stageDelays.zipWithIndex.map {
    case (delay, i) =>
      val stageParams = params.copy(inDataType = params.protoIQstages(i))
      val stage = withReset(cfgReset) { Module(new R22SDF(RadixParams(
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
        trimType      = params.resolvedStageTrimTypes(i),
      ))) }

      if (params.divBy2Reg)   stage.io.i_divBy2.get  := r_divBy2.get(i)
      if (params.overflowReg) io.o_overflow.get(i)   := stage.io.o_overflow.get

      // Twiddle address generation and inversion-signal logic
      if (!params.runTime && !stageHasTwiddleControl(i)) {
        clearStageTwiddleControl(i)
      } else if (params.runTime) {
        when(stageHasTwiddleControl(i).B && stageActive(i)) {
          when(stageOdd(i)) { wireNonTrivialTwiddle(stage, i) }
            .otherwise      { wireTrivialInversion(stage, i, delay) }
        }.otherwise { clearStageTwiddleControl(i) }
      } else {
        if (staticStageOdd(i)) wireNonTrivialTwiddle(stage, i) else wireTrivialInversion(stage, i, delay)
      }
      // Return the instantiated stage for connection in the data path
      stage
  }

  // Enable chain: stage 0 enabled by input, each stage enables next stage
  sdf_stages.scanLeft(io.in.fire) { case (en, s) => s.io.i_en := en; s.io.o_en }

  // Data path: connect stages in series, applying twiddle factors and/or inversion as needed
  val firstStageInput = if (params.runTime) Mux(stageActive(0), io.in.bits, 0.U.asTypeOf(params.inDataType)) else io.in.bits
  sdf_stages.map(_.io).zipWithIndex.foldLeft(firstStageInput) {
    case (prev_out, (stage, index)) =>
      w_stage_outputs(index) := stage.out
      if (isItDIF) {
        stage.in := (if (params.runTime)
          Mux(stageActive(index), Mux(index.U === firstActiveStage, io.in.bits, prev_out), 0.U.asTypeOf(stage.in)).asTypeOf(stage.in)
        else if (index == 0) io.in.bits else prev_out)

        val inverted = Utils.invertComplexData(stage.out, w_invert_signals(index))
        val out = Wire(stage.out.cloneType)

        if (index == 0 || index == noOfStages - 1) {
          val passData = if (index == 0) Mux(stageActive(index), inverted, 0.U.asTypeOf(stage.in)) else stage.out
          w_mul_outputs(index) := passData
          out := ShiftRegister(passData, complexMulLatency, true.B)
        } else if (index % 2 == 1) {
          w_mul_outputs(index) := Utils.complexMul(
            Mux(stageActiveOdd(index), stage.out, Mux(stageActive(index), w_stage_outputs(index + 1), 0.U.asTypeOf(stage.in))).asTypeOf(stage.in),
            Mux(stageActiveOdd(index), w_twiddles(index), Mux(stageActive(index), w_twiddles(index + 1), 0.U.asTypeOf(params.twiddleType))).asTypeOf(params.twiddleType),
            params.protoIQstages(index),
            params.numAddPipes, params.numMulPipes, params.resolvedTwiddleTrimTypes(index), params.use4Muls)
          out := bypassOrInverted(index, w_mul_outputs(index), inverted)
        } else {
          w_mul_outputs(index) := w_mul_outputs(index - 1).asTypeOf(w_mul_outputs(index))
          out := bypassOrInverted(index, w_mul_outputs(index), inverted)
        }
        out
      } else {
        val inverted    = Utils.invertComplexData(prev_out, w_invert_signals(index))
        val w_stage_out = Wire(stage.in.cloneType)
        stage.in := w_stage_out

        if (index == noOfStages - 1 || index == 0) {
          val passData = if (index == noOfStages - 1) Mux(stageActive(index), inverted, 0.U.asTypeOf(stage.in)) else prev_out
          w_mul_outputs(index) := passData
          w_stage_out := ShiftRegister(passData, complexMulLatency, true.B)
        } else if ((evenNoOfStages && index % 2 == 0) || (!evenNoOfStages && index % 2 == 1)) {
          val fbData = if (evenNoOfStages) w_stage_outputs(index - 2) else w_stage_outputs(index)
          val fbTw   = if (evenNoOfStages) w_twiddles(index - 1)      else w_twiddles(index + 1)
          w_mul_outputs(index) := Utils.complexMul(
            Mux(stageActiveOdd(index), prev_out.asTypeOf(stage.in), Mux(stageActive(index), fbData, 0.U.asTypeOf(stage.in))).asTypeOf(stage.in),
            Mux(stageActiveOdd(index), w_twiddles(index), Mux(stageActive(index), fbTw, 0.U.asTypeOf(params.twiddleType))).asTypeOf(params.twiddleType),
            params.protoIQstages(index),
            params.numAddPipes, params.numMulPipes, params.resolvedTwiddleTrimTypes(index), params.use4Muls)
          w_stage_out := bypassOrInverted(index, w_mul_outputs(index), inverted)
        } else {
          w_mul_outputs(index) := (if (evenNoOfStages) w_mul_outputs(index + 1) else w_mul_outputs(index - 1))
          w_stage_out := bypassOrInverted(index, w_mul_outputs(index), inverted)
        }
        w_stage_outputs(index)
      }
  }

  val finalStageIndex = (if (params.runTime && !isItDIF) activeStageCount - 1.U else (noOfStages - 1).U).asTypeOf(UInt(log2Ceil(noOfStages).W))
  val stageTail = if (isItDIF) ShiftRegister(w_stage_outputs.last, complexMulLatency, true.B) else w_stage_outputs(finalStageIndex)

  val lastStageValid = VecInit(sdf_stages.map(_.io.o_en))(finalStageIndex)
  val outputLast = outputSampleCount === (activeFftSize - 1.U)
  val outputValid = lastStageValid && !cfgLoad

  val outQueue = withReset(cfgReset) {
    Module(new Queue(UInt((stageTail.getWidth + 1).W), entries = 2 * outputQueueReserve, pipe = true, flow = true))
  }
  outQueue.io.enq.bits  := Cat(outputLast, stageTail.asUInt)
  outQueue.io.enq.valid := outputValid
  outQueue.io.deq.ready := io.out.ready && !cfgLoad

  val outQueueHasReserve = outQueue.io.count < outputQueueReserve.U
  io.in.ready  := !cfgLoad && outQueue.io.enq.ready && outQueueHasReserve
  io.out.valid := outQueue.io.deq.valid && !cfgLoad

  Utils.assignFftOutputByDirection(outQueue.io.deq.bits(stageTail.getWidth - 1, 0).asTypeOf(stageTail), w_output, fftOrIfft)
  io.o_last := io.out.valid && outQueue.io.deq.bits(stageTail.getWidth)

  when(cfgLoad) {
    outputSampleCount := 0.U
  }.elsewhen(outQueue.io.enq.fire) {
    outputSampleCount := Mux(outputLast, 0.U, outputSampleCount + 1.U)
  }

  io.out.bits.real := w_output.real >> outputShift
  io.out.bits.imag := w_output.imag >> outputShift
}
