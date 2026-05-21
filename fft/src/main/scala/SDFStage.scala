package opera.fft

import chisel3._
import chisel3.util._

object SDFStage {
  private[fft] def counterInit(params: RadixParams): Int =
    params.sdfRadix match {
      case Radix2  => 0
      case Radix22 => if (params.decimation == DIF) 0 else (params.stageSize / 2 + 1) & (params.stageSize - 1)
    }
}

class SDFStage(
  val params: RadixParams,
) extends Module {
  require(params.delay == params.stageSize / 2, s"SDFStage expects delay = stageSize / 2, got delay=${params.delay}, stageSize=${params.stageSize}")

  private val latency        = params.latency
  private val delay          = params.delay
  private val counterInit    = SDFStage.counterInit(params)
  private val controlLatency = if (params.decimation == DIF) 0 else latency
  private val outputValidLatency = if (params.decimation == DIF) latency + params.addPipeRegs else params.addPipeRegs

  // IOs
  val io: RadixIO = IO(new RadixIO(params))

  // Wires
  private val w_delay_in           = Wire(params.outDataType)
  private val w_delay_out          = Wire(params.outDataType)
  private val w_delay_butterfly    = Wire(params.inDataType)
  private val w_butterfly          = Butterfly(Seq(w_delay_butterfly, io.in.bits))
  private val (w_butterfly_scaled, w_overflow) = Utils.scaleButterfly(
    w_butterfly,
    params.outDataType,
    io.i_divBy2.getOrElse(params.divBy2.B),
    params.growEnable,
    params.trimType,
  )
  private val w_output_before_pipe = Wire(params.outDataType)

  // Registers
  private val r_counter = RegInit(counterInit.U(log2Ceil(params.stageSize).W))
  private val w_input_fire = io.in.fire
  private val r_delay_enable = Utils.delay(w_input_fire, controlLatency, false.B, io.out.ready)
  private val r_delay_fresh_count = RegInit(0.U(log2Ceil(delay + 1).W))
  private val w_delay_fresh = r_delay_fresh_count === delay.U
  private val w_delay_fresh_en = if (params.decimation == DIF) w_input_fire else r_delay_enable
  private val w_delay_write = r_delay_enable && io.out.ready
  private val w_in_first_half_d = Utils.delay(!r_counter(log2Ceil(params.stageSize) - 1), controlLatency, false.B, io.out.ready)

  // Counter
  io.in.ready := io.out.ready
  when(w_input_fire) {
    r_counter := r_counter + 1.U
  }
  io.o_counter := r_counter

  // Enable for next stage
  private val w_output_valid = w_delay_fresh && w_delay_fresh_en
  io.out.valid := Utils.delay(w_output_valid, outputValidLatency, false.B, io.out.ready)

  // Delay buffer connections and control
  w_delay_butterfly := w_delay_out
  w_delay_in := Mux(w_in_first_half_d, Utils.resizeComplexData(io.in.bits, params.outDataType), w_butterfly_scaled(1))
  w_delay_out := DelayBuffer(w_delay_in, delay, w_delay_write, params.singlePortMem, params.bufferAsMem)

  when(w_delay_fresh_en && io.out.ready && !w_delay_fresh) {
    r_delay_fresh_count := r_delay_fresh_count + 1.U
  }

  w_output_before_pipe := Mux(w_in_first_half_d, w_delay_out, w_butterfly_scaled.head)
  io.out.bits          := Utils.delay(w_output_before_pipe, params.addPipeRegs, io.out.ready)

  // Overflow register
  if (params.overflowReg) {
    io.o_overflow.get := w_overflow && w_output_valid
  }
}
