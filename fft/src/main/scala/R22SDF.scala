package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

class R22SDF[T <: Data: Real: Ring: BinaryRepresentation](
  val params: RadixParams[T],
) extends Module {
  // Info:
  print(f"decimation: ${params.decimation}, stage size: ${params.stageSize}, log ${log2Ceil(params.stageSize)}\n")
  // Variables
  private val latency     = params.latency
  private val delay       = params.delay
  private val cnt_init    = if (params.decimation == DIF) 0 else (params.stageSize / 2 + 1) & (params.stageSize - 1)
  private val shift_delay = if (params.decimation == DIF) 0 else latency
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
  // Registers
  private val r_counter = RegInit(cnt_init.U(log2Ceil(params.stageSize).W))
  private val r_en_delayed = ShiftRegister(io.i_en, shift_delay, true.B)
  // Counter
  r_counter := r_counter + io.i_en
  io.o_counter := r_counter
  // Enable for next stage
  io.o_en := ShiftRegisterWithReset(io.i_en, latency + params.addPipeRegs, false.B, reset.asBool, true.B)
  // Delay buffer connections and control
  w_delay_in        := Mux(w_delay_mux_ctrl, io.in, w_butterfly_scaled(1))
  w_delay_out       := DelayBuffer(w_delay_in, delay, r_en_delayed, params.singlePortMem, params.bufferAsMem)
  w_delay_mux_ctrl  := ShiftRegister(r_counter < delay, shift_delay, false.B, true.B)
  w_output_mux_ctrl := ShiftRegister(r_counter < delay.U, params.addPipeRegs + shift_delay, false.B, true.B)
  io.out := Mux(
    w_output_mux_ctrl,
    ShiftRegister(w_delay_out, params.addPipeRegs, true.B),
    ShiftRegister(w_butterfly_scaled.head, params.addPipeRegs, true.B)
  )
  // Scaling/growth logic
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
    // Check for underflow/overflow
    params.dataType.real match {
      case fp: FixedPoint =>
        w_overflow := Seq(w_butterfly.head.real, w_butterfly.head.imag, w_butterfly.last.real, w_butterfly.last.imag).map(data => {
          val overflow =
            !data.isSignNegative && (BinaryRepresentation[T].shr(data, data.getWidth - 2) === Real[T].fromDouble(1 / math.pow(2, fp.binaryPoint.get)))
          val underflow =
            data.isSignNegative && (BinaryRepresentation[T].shr(data, data.getWidth - 2) === Real[T].fromDouble(0.0))
          overflow || underflow
        }).foldLeft(false.B)(_ || _)
      case _ =>
        w_overflow := false.B
    }
    // Scale butterfly or don't
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
  // Overflow register
  if (params.overflowReg) {
    io.o_overflow.get := w_overflow
  }
}
