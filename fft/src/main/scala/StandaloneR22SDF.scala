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
  private val isItDIF           = params.decimation == DIF      // If not DIF then DIT
  private val noOfStages        = log2Ceil(params.fftSize)    // Number of stages
  private val evenNoOfStages    = noOfStages % 2 == 0           // Check if number of stages is even
  private val sortedStages      = if (isItDIF) (0 until noOfStages).reverse else 0 until noOfStages
  private val stageDelays       = sortedStages.map(i => 1 << i) // Stage delays
  // Latencies
  private val complexMulLatency = if (params.use4Muls) params.numAddPipes + params.numMulPipes else 2 * params.numAddPipes + params.numMulPipes
  private val outputLatency     = params.numAddPipes + complexMulLatency
  private val latency           = (params.numAddPipes + complexMulLatency) * noOfStages
  // Registers
  private val r_num_stages      = RegInit(noOfStages.U((log2Ceil(noOfStages) + 1).W))
  private val r_data_in_counter = RegInit(0.U(noOfStages.W))
  private val r_fft_or_ifft     = RegInit(true.B)
  private val r_counters_msb    = RegInit(VecInit(Seq.fill(noOfStages)(false.B)))
  private val r_counter_delay   = RegInit(0.U(noOfStages.W))
  private val r_pipeline_full   = RegInit(false.B)
  // Wires
  private val w_output           = Wire(params.outDataType)
  private val w_stage_delays     = VecInit(stageDelays.scanLeft(0)(_ + _).map(_.U))
  private val w_delay_offset     = Wire(UInt(w_stage_delays.last.getWidth.W))
  private val w_i_en_last_stage  = Wire(Bool())
  private val w_o_en_last_stage  = Wire(Bool())
  private val w_last_stage_valid = Wire(Bool())
  private val isStageOdd         = Wire(Vec(noOfStages, Bool()))
  private val isStageActive      = Wire(Vec(noOfStages, Bool()))
  private val isActiveOdd        = Wire(Vec(noOfStages, Bool()))
  // Calculate if the difference between maximum number of stages and runtime number of stages is odd
  private val isShiftedAddress   = r_num_stages(0) ^ noOfStages.U.extract(0)

  private val w_invert_signals = Wire(Vec(noOfStages, Bool()))
  private val w_stage_outputs  = Wire(Vec(noOfStages, params.outDataType)) // Outputs of the all stages
  private val w_stage_tail     = Wire(params.outDataType)                  // Output of the last stage
  private val w_fft_size       = Wire(UInt(log2Ceil(params.fftSize + 1).W))
  private val w_mul_outputs    = Wire(MixedVec((0 until noOfStages).map { i => params.protoIQstages(i) }))

  // logic for last out signal generation
  private val r_data_out_counter   = RegInit(0.U(noOfStages.W))
  private val w_chirp_end          = (r_data_out_counter === (w_fft_size - 1.U)) && io.out.fire
  
  // Define these values
  if (isItDIF)
    w_delay_offset := w_stage_delays((noOfStages.U - r_num_stages).asTypeOf(UInt(log2Ceil(w_stage_delays.length).W)))
  else
    w_delay_offset := 0.U
  if (params.runTime)
    w_fft_size := 1.U << r_num_stages
  else
    w_fft_size := params.fftSize.U

  // Twiddle factors
  private val noOfTwiddles = (noOfStages - 1) / 2
  private val w_lookup_twiddles: Vec[DspComplex[T]] = Wire(Vec(noOfTwiddles, params.twiddleType))
  private val w_lookup_address = Wire(Vec(noOfTwiddles, UInt(noOfStages.W)))
  private val w_twiddles: Vec[DspComplex[T]] = Wire(Vec(noOfStages, params.twiddleType))
  private val w_twiddle_address = Wire(Vec(noOfStages, UInt(noOfStages.W)))
  // Quarter wave sine LUT
  private val LUT = QuarterWaveSineLUT[T](1 << noOfStages, params.twiddleType)

  // Get values from LookUp table
  w_lookup_address.zipWithIndex.foreach {
    case (address, i) =>
      val even_offset = if (evenNoOfStages) 1 else 0
      val (offset_0, offset_1) = if (isItDIF) (2, 1) else (even_offset, even_offset + 1)
      address := Mux(
        isShiftedAddress,
        w_twiddle_address((i << 1) + offset_0),
        w_twiddle_address((i << 1) + offset_1)
      )
  }

  (0 until noOfStages by 2).dropRight(1).zipWithIndex.foreach {
    case (m, i) =>
      val stageN = 1 << (noOfStages - m)
      print(f"stageN:$stageN\n")
      val j = if (isItDIF) i else noOfTwiddles - 1 - i
      w_lookup_twiddles(j) := TwiddleFromLUT[T](w_lookup_address(j)(log2Ceil(stageN)-1, 0), stageN, 1 << noOfStages, LUT)
  }

  // Calculate which stages are active and which stages are odd
  isStageActive.zip(isStageOdd).zipWithIndex.foreach {
    case ((active, odd), i) =>
      val index = if (isItDIF) i else noOfStages - i - 1
      active := Mux(noOfStages.U - r_num_stages <= index.U, true.B, false.B)
      odd := (index.U - (noOfStages.U - r_num_stages))(0) // check only the lower bit, if 1 then the result is odd
  }
  isActiveOdd.zipWithIndex.foreach { case (ao, i) => ao := isStageActive(i) && isStageOdd(i) }

  // register initialization
  r_num_stages := io.i_size.getOrElse(noOfStages.U) // number of stages
  r_fft_or_ifft := io.i_fft_or_ifft.getOrElse(params.direction.B)

  /** ************************************************************************************* */
  /* Instantiate stages, connect registers and calculate twiddle address                    */
  /** ************************************************************************************* */
  val sdf_stages: Seq[R22SDF[T]] = stageDelays.zipWithIndex.map {
    case (delay, i) =>
      val stageParams = params.copy(inDataType = params.protoIQstages(i))
      val useGrow = stageParams.expandLogic(i) == 1
      val stage = Module(
        new R22SDF(
          RadixParams(
            dataType = stageParams.inDataType,
            twiddleType = stageParams.twiddleType,
            stageSize = if (stageParams.decimation == DIF) params.fftSize >> i else 2 << i,
            decimation = stageParams.decimation,
            overflowReg = stageParams.overflowReg,
            divBy2Reg = stageParams.divBy2Reg,
            divBy2 = params.divBy2(i),
            growEnable = useGrow,
            latency = if (params.use4Muls) {
              params.numAddPipes + params.numMulPipes
            } else {
              2 * params.numAddPipes + params.numMulPipes
            },
            addPipeRegs = params.numAddPipes,
            mulPipeRegs = params.numMulPipes,
            dspMul4 = params.use4Muls,
            delay = delay,
            bufferAsMem = params.minSRAMdepth < delay,
            singlePortMem = params.singlePortSRAM,
            trimType = stageParams.trimType,
          ))
      )
      // Connect registers if they are enabled
      if (params.divBy2Reg) { stage.io.i_divBy2.get := io.i_divBy2.get(i) }
      if (params.overflowReg) { io.o_overflow.get(i) := stage.io.o_overflow.get }
      // Find enable of last stage
      w_i_en_last_stage := stage.io.i_en && (i.U === r_num_stages - 1.U)
      w_o_en_last_stage := stage.io.o_en && (i.U === r_num_stages - 1.U)
      // Calculate address of twiddle factors
      val noTwining = if (isItDIF) (noOfStages - 1).U === i.U else i.U === 0.U
      when(!noTwining && isStageActive(i)) {
        // Non-trivial twiddle factors
        when(isStageOdd(i)) {
          w_invert_signals(i) := false.B
          r_counters_msb(i) := r_counters_msb(i) + (stage.io.i_en && stage.io.o_counter === ((1 << stage.io.o_counter.getWidth) - 1).U)
          if (isItDIF) {
            val address = Cat(r_counters_msb(i), stage.io.o_counter) + (1 << (noOfStages - i - 1)).U
            w_twiddle_address(i) := Mux(isShiftedAddress, address << 1, address)
            if (noOfTwiddles == 1) {
              w_twiddles(i) := ShiftRegister(w_lookup_twiddles(0), params.numAddPipes, true.B)
            } else if (noOfTwiddles > 1) {
              val twIdx = log2Ceil(noOfTwiddles)
              val indexToTwiddles: UInt = Mux(
                isShiftedAddress,
                ((i >> 1).U - 1.U).asTypeOf(UInt(twIdx.W)),
                (i.U >> 1.U).asTypeOf(UInt(twIdx.W))
              )
              w_twiddles(i) := ShiftRegister(w_lookup_twiddles(indexToTwiddles), params.numAddPipes, true.B)
            } else {
              w_twiddles(i) := 0.U.asTypeOf(w_twiddles(i))
            }
          } else {
            val address = Cat(r_counters_msb(i), stage.io.o_counter) + (1 << (i + 1)).U
            w_twiddle_address(i) := Mux(isShiftedAddress, address << 1, address)
            if (noOfTwiddles == 1) {
              w_twiddles(i) := w_lookup_twiddles(0)
            } else if (noOfTwiddles > 1) {
              val twIdx = log2Ceil(noOfTwiddles)
              val indexToTwiddles = (if (evenNoOfStages) ((i.U - 1.U) >> 1.U) else (i.U >> 1.U)).asTypeOf(UInt(twIdx.W))
              w_twiddles(i) := w_lookup_twiddles(indexToTwiddles)
            } else {
              w_twiddles(i) := 0.U.asTypeOf(w_twiddles(i))
            }
          }
        }.otherwise { // Trivial twiddle factors
          if (isItDIF)
            w_invert_signals(i) := ShiftRegister(
              Mux(stage.io.o_counter < delay.U, Mux(stage.io.o_counter < (delay.U >> 1), false.B, true.B), false.B),
              params.numAddPipes,
              true.B
            )
          else
            w_invert_signals(i) := Mux(
              stage.io.o_counter < delay.U,
              false.B,
              Mux(stage.io.o_counter < (delay * 3 / 2).U, false.B, true.B)
            )
          w_twiddle_address(i) := 0.U
          w_twiddles(i) := 0.U.asTypeOf(w_twiddles(i))
        }
      }.otherwise {
        w_invert_signals(i) := false.B
        w_twiddle_address(i) := 0.U
        w_twiddles(i) := 0.U.asTypeOf(w_twiddles(i))
      }
      // Return r22 stage
      stage
  }
  // Connect enables to each stage
  private val w_en: Bool = io.in.fire
  sdf_stages.scanLeft(w_en) { case (i_en, r22) =>
    r22.io.i_en := i_en
    r22.io.o_en
  }

  r_data_out_counter := r_data_out_counter + io.out.fire
  r_data_in_counter  := r_data_in_counter + w_en

  /*************************************************************************************** */
  /* Inputs to stages and twiddles                                                         */
  /*************************************************************************************** */
  sdf_stages.map(_.io).zipWithIndex.foldLeft(Mux(isStageActive(0), io.in.bits, 0.U.asTypeOf(params.inDataType))) {
    case (prev_out, (stage, index)) =>
      val w_stage_out = Wire(stage.in.cloneType)
      val out = Wire(stage.out.cloneType)
      w_stage_outputs(index) := stage.out

      val w_inverted_data = Wire(stage.out.cloneType)
      val bpos = params.protoIQstages(index).real.cloneType match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      }
      // Find which outputs we should multiply with twiddle factors
      if (isItDIF) { // DIF
        // Connect adequate inputs to active stages and tie 0 to inactive ones
        when(isStageActive(index)) {
          when(index.U === (noOfStages.U - r_num_stages)) { stage.in := io.in.bits } // Pass io.in to first stage
          .otherwise { stage.in := prev_out } // Otherwise pass the previous stages output
        }.otherwise {
          stage.in := 0.U.asTypeOf(stage.in)
        }
        // Check if data needs to be multiplied by -1j
        w_inverted_data.real := Mux(w_invert_signals(index),  stage.out.imag, stage.out.real)
        w_inverted_data.imag := Mux(w_invert_signals(index), -stage.out.real, stage.out.imag)
        // Multiplication with twiddle factor if necessary
        if (index == 0) { // First stage
          when(isStageActive(index) === true.B) {
            w_mul_outputs(index) := w_inverted_data
          }.otherwise {
            w_mul_outputs(index) := 0.U.asTypeOf(stage.in)
          }
          w_stage_out := ShiftRegister(w_mul_outputs(index), complexMulLatency, true.B)
        } else {
          if (index == (noOfStages - 1)) { // last stage
            w_stage_out := ShiftRegister(stage.out, complexMulLatency, true.B)
            w_mul_outputs(index) := stage.out
          } else if (index % 2 == 1) { // stage with non trivial multiplier
            val w_mul_input = Mux(
              isActiveOdd(index),
              stage.out,
              Mux(isStageActive(index), w_stage_outputs(index + 1), 0.U.asTypeOf(stage.in))
            ).asTypeOf(params.inDataType)
            val w_twiddle = Mux(
              isActiveOdd(index),
              w_twiddles(index),
              Mux(isStageActive(index), w_twiddles(index + 1), 0.U.asTypeOf(params.twiddleType))
            ).asTypeOf(params.twiddleType)
            // Twiddle factor multiplication
            DspContext.alter(DspContext.current.copy(numAddPipes = params.numAddPipes, numMulPipes = params.numMulPipes)) {
              w_mul_outputs(index) := DspContext.withTrimType(params.trimType) {
                DspContext.alter(
                  DspContext.current.copy(trimType = params.trimType, overflowType = Grow, complexUse4Muls = params.use4Muls)
                ) { w_mul_input.context_*(w_twiddle) }.trimBinary(bpos)
              }
            }
            w_stage_out := Mux(
              !isActiveOdd(index),
              ShiftRegister(w_inverted_data, complexMulLatency, true.B),
              w_mul_outputs(index)
            )
          } else {
            w_mul_outputs(index) := w_mul_outputs(index - 1).asTypeOf(w_mul_outputs(index))
            w_stage_out := Mux(
              !isActiveOdd(index),
              ShiftRegister(w_inverted_data, complexMulLatency, true.B),
              w_mul_outputs(index)
            )
          }
        }
        out := w_stage_out
      }
      // DIT
      else {
        stage.in := w_stage_out
        w_inverted_data.real := Mux(w_invert_signals(index),  prev_out.imag, prev_out.real)
        w_inverted_data.imag := Mux(w_invert_signals(index), -prev_out.real, prev_out.imag)
        if (index == (noOfStages - 1)) {
          when(isStageActive(index) === true.B) {
            w_mul_outputs(index) := w_inverted_data
          }.otherwise {
            w_mul_outputs(index) := 0.U.asTypeOf(stage.in)
          }
          w_stage_out := ShiftRegister(w_mul_outputs(index), complexMulLatency, true.B)
        }
        else if (index == 0) {
          w_stage_out := ShiftRegister(prev_out, complexMulLatency, true.B)
          w_mul_outputs(index) := prev_out
        }
        // Non-trivial twiddle factors
        else if ((evenNoOfStages && index % 2 == 0) || (!evenNoOfStages && index % 2 == 1)) {
          val w_mul_input = Mux(
            isActiveOdd(index),
            prev_out.asTypeOf(stage.in),
            Mux(
              isStageActive(index),
              if (evenNoOfStages) w_stage_outputs(index - 2) else w_stage_outputs(index),
              0.U.asTypeOf(stage.in))
          ).asTypeOf(params.inDataType)
          val w_twiddle = Mux(
            isActiveOdd(index),
            w_twiddles(index),
            Mux(isStageActive(index),
              if (evenNoOfStages) w_twiddles(index - 1) else w_twiddles(index + 1),
              0.U.asTypeOf(params.twiddleType))
          ).asTypeOf(params.twiddleType)
          // Twiddle factor multiplication
          DspContext.alter(DspContext.current.copy(numAddPipes = params.numAddPipes, numMulPipes = params.numMulPipes)) {
            w_mul_outputs(index) := DspContext.withTrimType(params.trimType) {
              DspContext.alter(
                DspContext.current.copy(trimType = params.trimType, overflowType = Grow, complexUse4Muls = params.use4Muls)
              ) { w_mul_input.context_*(w_twiddle) }.trimBinary(bpos)
            }
          }
          w_stage_out := Mux(
            !isActiveOdd(index),
            ShiftRegister(w_inverted_data, complexMulLatency, true.B),
            w_mul_outputs(index)
          )
        }
        else {
          if (evenNoOfStages) w_mul_outputs(index) := w_mul_outputs(index + 1)
          else w_mul_outputs(index) := w_mul_outputs(index - 1)
          w_stage_out := Mux(
            !isActiveOdd(index),
            ShiftRegister(w_inverted_data, complexMulLatency, true.B),
            w_mul_outputs(index)
          )
        }
        out := w_stage_outputs(index)
      }
      out
  }

  /*************************************************************************************** */
  /* Ready/Valid logic                                                                     */
  /*************************************************************************************** */
  r_counter_delay := r_counter_delay + w_i_en_last_stage
  when(r_counter_delay === (w_fft_size - 1.U) && w_i_en_last_stage) { r_pipeline_full := true.B }
  // Find last stage, depends on DIF/DIT
  if (isItDIF) {w_stage_tail := ShiftRegister(w_stage_outputs.last, complexMulLatency, true.B)}
  else { w_stage_tail := w_stage_outputs((r_num_stages - 1.U).asTypeOf(UInt(log2Ceil(noOfStages).W))) }
  // Generate valid for the last stage
  w_last_stage_valid := ShiftRegisterWithReset(
    r_pipeline_full && w_i_en_last_stage,
    outputLatency,
    resetData = false.B,
    reset = reset.asBool,
    en = true.B
  )
  io.in.ready := ~w_last_stage_valid
  private val shift_output = noOfStages.U * (params.expandLogic.sum != 0 || params.divBy2Reg).B
  if (latency == 0) {
    when(r_fft_or_ifft === true.B) {
      w_output := w_stage_tail
    }.otherwise {
      w_output.real := w_stage_tail.imag
      w_output.imag := w_stage_tail.real
    }
    io.out.valid := w_last_stage_valid
  } else {
    val output_buffer = Module(new Queue(chiselTypeOf(sdf_stages.last.io.out), entries = latency + 1, pipe = true, flow = true))
    output_buffer.io.enq.bits := w_stage_tail
    output_buffer.io.enq.valid := w_last_stage_valid
    output_buffer.io.deq.ready := io.out.ready
    io.in.ready := !r_pipeline_full || output_buffer.io.enq.ready
    when(r_fft_or_ifft === true.B) {
      w_output := output_buffer.io.deq.bits
    }.otherwise {
      w_output.real := output_buffer.io.deq.bits.imag
      w_output.imag := output_buffer.io.deq.bits.real
    }
    io.out.valid := output_buffer.io.deq.valid
  }
  io.out.bits.real := w_output.real >> shift_output
  io.out.bits.imag := w_output.imag >> shift_output
  io.o_last := w_chirp_end
}
