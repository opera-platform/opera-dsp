package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

class R2FFT[T <: Data: Real: BinaryRepresentation](val params: FFTParams[T]) extends Module with HasIO[T] {
  val io: FFTIO[T] = IO(new FFTIO(params))

  // Constants
  private val isItDIF         = params.decimation == DIF
  private val noOfStages      = log2Ceil(params.fftSize)
  private val stageDelays     = (if (isItDIF) (0 until noOfStages).reverse else 0 until noOfStages).map(i => 1 << i)
  private val stageSizes      = stageDelays.map(_ << 1)
  private val stageCountWidth = log2Ceil(noOfStages) + 1
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
  private val r_divBy2          = if (params.divBy2Reg) Some(RegInit(VecInit(params.divBy2.toIndexedSeq.map(_.B)))) else None
  private val outputCounterInit = 1.U(log2Ceil(params.fftSize).W)
  private val outputSampleCount = RegInit(outputCounterInit)

  // Wires
  private val w_output        = Wire(params.outDataType)
  private val w_stage_outputs = Wire(Vec(noOfStages, params.outDataType))
  private val w_chain_outputs = Wire(Vec(noOfStages, params.outDataType))
  private val w_mul_outputs   = Wire(MixedVec((0 until noOfStages).map(i => params.protoIQstages(i))))
  private val w_twiddles      = Wire(Vec(noOfStages, params.twiddleType))
  private val w_twiddle_en    = Wire(Vec(noOfStages, Bool()))

  // Twiddle factor infrastructure
  private val LUT = if (params.fftSize >= 4) Some(QuarterWaveSineLUT[T](1 << noOfStages, params.twiddleType)) else None

  // Runtime-derived signals
  private val cfgLoad          = io.i_load_cfg.getOrElse(false.B)
  private val cfgReset         = reset.asBool || cfgLoad
  private val activeStageCount = r_num_stages.getOrElse(noOfStages.U(stageCountWidth.W))
  private val firstActiveStage = noOfStages.U - activeStageCount
  private val activeFftSize    = if (params.runTime) 1.U << activeStageCount else params.fftSize.U
  private val fftOrIfft        = r_fft_or_ifft.getOrElse(params.direction.B)
  private val outputShift      = if (params.expandLogic.sum != 0 || params.divBy2Reg) noOfStages.U else 0.U

  // Stage helpers
  private def stageActive(i: Int): Bool = if (params.runTime) firstActiveStage <= stageRoleIndices(i).U else true.B
  private def stageHasActiveTwiddle(i: Int): Bool = if (params.runTime) stageActive(i) && stageHasTwiddle(i).B else stageHasTwiddle(i).B

  // Twiddle helpers
  private def zeroTwiddle: DspComplex[T] = 0.U.asTypeOf(params.twiddleType)

  private def stageTwiddle(i: Int, counter: UInt): DspComplex[T] = {
    if (stageSizes(i) >= 4) {
      val address = Wire(UInt(log2Ceil(stageSizes(i)).W))
      address := counter
      Radix2TwiddleFromLUT[T](address, stageSizes(i), 1 << noOfStages, LUT.get)
    } else {
      zeroTwiddle
    }
  }

  private def delayedBypass(data: DspComplex[T]): DspComplex[T] =
    ShiftRegister(data, complexMulLatency, true.B)

  private def twiddleOrBypass(index: Int, data: DspComplex[T], twiddle: DspComplex[T], twiddleEn: Bool): DspComplex[T] = {
    val mul = Wire(data.cloneType)
    val pass = Wire(data.cloneType)
    mul := Utils.complexMul(
      data,
      twiddle,
      params.protoIQstages(index),
      params.numAddPipes,
      params.numMulPipes,
      params.resolvedTwiddleTrimTypes(index),
      params.use4Muls
    )
    pass := delayedBypass(data)
    Mux(ShiftRegister(twiddleEn, complexMulLatency, true.B), mul, pass)
  }

  when(cfgLoad) {
    if (params.runTime)      r_num_stages.get  := io.i_size.get
    if (params.directionReg) r_fft_or_ifft.get := io.i_fft_or_ifft.get
    if (params.divBy2Reg)    r_divBy2.get      := io.i_divBy2.get
  }

  // Stage instantiation and twiddle control
  val sdf_stages: Seq[R2SDF[T]] = stageDelays.zipWithIndex.map {
    case (delay, i) =>
      val stageParams = params.copy(inDataType = params.protoIQstages(i))
      val stage = withReset(cfgReset) { Module(new R2SDF(RadixParams(
        dataType      = stageParams.inDataType,
        twiddleType   = stageParams.twiddleType,
        stageSize     = stageSizes(i),
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

      if (params.divBy2Reg)   stage.io.i_divBy2.get := r_divBy2.get(i)
      if (params.overflowReg) io.o_overflow.get(i)  := stage.io.o_overflow.get

      val lowCounter = if (delay == 1) 0.U(1.W) else stage.io.o_counter(log2Ceil(delay) - 1, 0)
      if (isItDIF) {
        val rawEn = stageHasActiveTwiddle(i) && !stage.io.o_counter(log2Ceil(stageSizes(i)) - 1) && lowCounter =/= 0.U
        w_twiddle_en(i) := ShiftRegister(rawEn, params.numAddPipes, true.B)
        w_twiddles(i)   := ShiftRegister(stageTwiddle(i, lowCounter), params.numAddPipes, true.B)
      } else {
        val rawEn = stageHasActiveTwiddle(i) && stage.io.o_counter > delay.U
        w_twiddle_en(i) := rawEn
        w_twiddles(i)   := stageTwiddle(i, lowCounter)
      }

      stage
  }

  // Enable chain: stage 0 enabled by input, each stage enables next stage
  sdf_stages.scanLeft(io.in.fire) { case (en, s) => s.io.i_en := en; s.io.o_en }

  // Data path: connect stages in series, applying twiddle factors as needed
  val firstStageInput = if (params.runTime) Mux(stageActive(0), io.in.bits, 0.U.asTypeOf(params.inDataType)) else io.in.bits
  sdf_stages.map(_.io).zipWithIndex.foldLeft(firstStageInput) {
    case (prevOut, (stage, index)) =>
      val activeInput = if (params.runTime) {
        val selectedInput = Mux(index.U === firstActiveStage, io.in.bits.asTypeOf(stage.in), prevOut.asTypeOf(stage.in))
        Mux(stageActive(index), selectedInput, 0.U.asTypeOf(stage.in)).asTypeOf(stage.in)
      } else if (index == 0) {
        io.in.bits
      } else {
        prevOut
      }

      if (isItDIF) {
        stage.in := activeInput
        w_stage_outputs(index) := stage.out
        w_mul_outputs(index) := Utils.complexMul(
          stage.out,
          w_twiddles(index),
          params.protoIQstages(index),
          params.numAddPipes,
          params.numMulPipes,
          params.resolvedTwiddleTrimTypes(index),
          params.use4Muls
        )
        w_chain_outputs(index) := Mux(
          ShiftRegister(w_twiddle_en(index), complexMulLatency, true.B),
          w_mul_outputs(index),
          delayedBypass(stage.out)
        ).asTypeOf(w_chain_outputs(index))
      } else {
        val inputToStage = twiddleOrBypass(index, activeInput.asTypeOf(stage.in), w_twiddles(index), w_twiddle_en(index))
        stage.in := inputToStage
        w_stage_outputs(index) := stage.out
        w_mul_outputs(index)   := inputToStage
        w_chain_outputs(index) := stage.out
      }

      w_chain_outputs(index)
  }

  val finalStageIndex = (if (params.runTime && !isItDIF) activeStageCount - 1.U else (noOfStages - 1).U).asTypeOf(UInt(log2Ceil(noOfStages).W))
  val stageTail = w_chain_outputs(finalStageIndex)
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
    outputSampleCount := outputCounterInit
  }.elsewhen(outQueue.io.enq.fire) {
    outputSampleCount := Mux(outputLast, 0.U, outputSampleCount + 1.U)
  }

  io.out.bits.real := w_output.real >> outputShift
  io.out.bits.imag := w_output.imag >> outputShift
}
