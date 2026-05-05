package opera.fft

import chisel3._
import chisel3.util._

class R22SDF(
  val params: RadixParams,
) extends Module {
  require(params.delay == params.stageSize / 2, s"R22SDF expects delay = stageSize / 2, got delay=${params.delay}, stageSize=${params.stageSize}")

  private val latency        = params.latency
  private val delay          = params.delay
  private val counterInit    = if (params.decimation == DIF) 0 else (params.stageSize / 2 + 1) & (params.stageSize - 1)
  private val controlLatency = if (params.decimation == DIF) 0 else latency

  // IOs
  val io: RadixIO = IO(new RadixIO(params))

  // Wires
  private val w_delay_mux_ctrl     = Wire(Bool())
  private val w_delay_in           = Wire(params.outDataType)
  private val w_delay_out          = Wire(params.outDataType)
  private val w_delay_butterfly    = Wire(params.inDataType)
  private val w_butterfly          = Butterfly(Seq(w_delay_butterfly, io.in))
  private val (w_butterfly_scaled, w_overflow) = Utils.scaleButterfly(
    w_butterfly,
    params.outDataType,
    io.i_divBy2.getOrElse(params.divBy2.B),
    params.growEnable,
    params.trimType,
  )
  private val w_output_mux_ctrl    = Wire(Bool())
  private val w_output_before_pipe = Wire(params.outDataType)

  // Registers
  private val r_counter = RegInit(counterInit.U(log2Ceil(params.stageSize).W))
  private val r_delay_enable = ShiftRegister(io.i_en, controlLatency, false.B, true.B)
  private val r_delay_fresh_count = RegInit(0.U(log2Ceil(delay + 1).W))
  private val w_delay_fresh = r_delay_fresh_count === delay.U
  private val w_delay_fresh_en = if (params.decimation == DIF) io.i_en else r_delay_enable

  // Counter
  r_counter := r_counter + io.i_en
  io.o_counter := r_counter

  // Enable for next stage
  private val outputValidLatency = if (params.decimation == DIF) latency + params.addPipeRegs else params.addPipeRegs
  private val w_output_valid = w_delay_fresh && w_delay_fresh_en
  io.o_en := ShiftRegister(w_output_valid, outputValidLatency, false.B, true.B)

  // Delay buffer connections and control
  w_delay_butterfly := w_delay_out
  w_delay_in := Mux(w_delay_mux_ctrl, Utils.resizeComplex(io.in, params.outDataType), w_butterfly_scaled(1))
  w_delay_out := DelayBuffer(w_delay_in, delay, r_delay_enable, params.singlePortMem, params.bufferAsMem)

  when(w_delay_fresh_en && !w_delay_fresh) {
    r_delay_fresh_count := r_delay_fresh_count + 1.U
  }

  private val w_in_first_half = !r_counter(log2Ceil(params.stageSize) - 1)
  w_delay_mux_ctrl     := ShiftRegister(w_in_first_half, controlLatency, false.B, true.B)
  w_output_mux_ctrl    := ShiftRegister(w_in_first_half, controlLatency, false.B, true.B)
  w_output_before_pipe := Mux(w_output_mux_ctrl, w_delay_out, w_butterfly_scaled.head)
  io.out               := ShiftRegister(w_output_before_pipe, params.addPipeRegs)

  // Overflow register
  if (params.overflowReg) {
    io.o_overflow.get := w_overflow && w_output_valid
  }
}
