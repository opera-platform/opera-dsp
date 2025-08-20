package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

class R22SDF[T <: Data: Real: Ring: BinaryRepresentation](
  val params: RadixParams[T],
) extends Module {
  // Variables
  private val latency = params.latency
  private val delay   = params.delay
  // IOs
  val io: RadixIO[T] = IO(new RadixIO(params))
  // Wires
  private val w_delay_mux_ctrl   = Wire(Bool())
  private val w_delay_in         = Wire(params.dataType)
  private val w_delay_out        = Wire(params.dataType)
  private val w_butterfly        = Butterfly[T](Seq(w_delay_out, io.in))
  private val w_butterfly_scaled = Seq.fill(2)(Wire(params.dataType))
  private val w_output_mux_ctrl  = Wire(Bool())
  private val w_overflow         = Wire(Bool())

  w_delay_in := Mux(w_delay_mux_ctrl, io.in, w_butterfly_scaled(1))
  if (params.decimation == DIF) {
    w_delay_mux_ctrl  := io.i_counter < delay.U
    w_delay_out       := DelayBuffer(w_delay_in, delay, io.i_en, params.singlePortMem, params.bufferAsMem)
    w_output_mux_ctrl := ShiftRegister(w_delay_mux_ctrl, params.addPipeRegs, false.B, true.B)
  } else {
    w_delay_mux_ctrl  := ShiftRegister(io.i_counter < delay, latency, false.B, true.B)
    w_delay_out       := DelayBuffer(w_delay_in, delay, ShiftRegister(io.i_en, latency, true.B), params.singlePortMem, params.bufferAsMem)
    w_output_mux_ctrl := ShiftRegister(io.i_counter < delay.U, params.addPipeRegs + latency, false.B, true.B)
  }
  io.out := Mux(
    w_output_mux_ctrl,
    ShiftRegister(w_delay_out, params.addPipeRegs, true.B),
    ShiftRegister(w_butterfly_scaled.head, params.addPipeRegs, true.B)
  )
  
  if (params.growEnable) {
    w_butterfly_scaled.head := w_butterfly.head
    w_butterfly_scaled.last := w_butterfly.last
    w_overflow              := false.B
  } else {
    val butterfly_div_2 = Seq(
      DspContext.alter(DspContext.current.copy(trimType = params.trimType, binaryPointGrowth = 0)) {
        w_butterfly.head.div2(1)
      },
      DspContext.alter(DspContext.current.copy(trimType = params.trimType, binaryPointGrowth = 0)) {
        w_butterfly.last.div2(1)
      }
    )

    //TODO: overflow, placeholder
    w_overflow := false.B

    w_butterfly_scaled.head := Mux(
      io.i_divBy2.getOrElse(params.divBy2.B),
      butterfly_div_2.head,
      w_butterfly.head.asTypeOf(params.dataType)
    )
    w_butterfly_scaled.last := Mux(
      io.i_divBy2.getOrElse(params.divBy2.B),
      butterfly_div_2.last,
      w_butterfly.last.asTypeOf(params.dataType)
    )
  }

  if (params.overflowReg) {
    io.o_overflow.get := w_overflow
  }
}
