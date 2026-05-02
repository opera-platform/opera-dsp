package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

class R22SDF[T <: Data: Real: Ring: BinaryRepresentation](
  val params: RadixParams[T],
) extends Module {
  require(params.delay == params.stageSize / 2, s"R22SDF expects delay = stageSize / 2, got delay=${params.delay}, stageSize=${params.stageSize}")

  private val latency        = params.latency
  private val delay          = params.delay
  private val counterInit    = if (params.decimation == DIF) 0 else (params.stageSize / 2 + 1) & (params.stageSize - 1)
  private val controlLatency = if (params.decimation == DIF) 0 else latency

  // IOs
  val io: RadixIO[T] = IO(new RadixIO(params))

  // Wires
  private val w_delay_mux_ctrl     = Wire(Bool())
  private val w_delay_in           = Wire(params.dataType)
  private val w_delay_out          = Wire(params.dataType)
  private val w_butterfly          = Butterfly[T](Seq(w_delay_out, io.in))
  private val w_butterfly_scaled   = Seq.fill(2)(Wire(params.dataType))
  private val w_output_mux_ctrl    = Wire(Bool())
  private val w_output_before_pipe = Wire(params.dataType)
  private val w_overflow           = Wire(Bool())

  // Registers
  private val r_counter = RegInit(counterInit.U(log2Ceil(params.stageSize).W))
  private val r_delay_enable = ShiftRegister(io.i_en, controlLatency, true.B)

  // Counter
  r_counter := r_counter + io.i_en
  io.o_counter := r_counter

  // Enable for next stage
  io.o_en := ShiftRegister(io.i_en, latency + params.addPipeRegs, false.B, true.B)

  // Delay buffer connections and control
  w_delay_in        := Mux(w_delay_mux_ctrl, io.in, w_butterfly_scaled(1))
  w_delay_out       := DelayBuffer(w_delay_in, delay, r_delay_enable, params.singlePortMem, params.bufferAsMem)

  private val inFirstHalf = !r_counter(log2Ceil(params.stageSize) - 1)
  w_delay_mux_ctrl  := ShiftRegister(inFirstHalf, controlLatency, false.B, true.B)
  w_output_mux_ctrl := ShiftRegister(inFirstHalf, controlLatency, false.B, true.B)
  w_output_before_pipe := Mux(w_output_mux_ctrl, w_delay_out, w_butterfly_scaled.head)
  io.out := ShiftRegister(w_output_before_pipe, params.addPipeRegs, true.B)

  // Scaling/growth logic. growEnable lets this stage keep the butterfly's
  // extra sign bit; otherwise divBy2 selects rounded scaling or pass-through.
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

    params.dataType.real match {
      case _: FixedPoint =>
        w_overflow := Seq(w_butterfly.head.real, w_butterfly.head.imag, w_butterfly.last.real, w_butterfly.last.imag).map { data =>
          val u = data.asUInt
          u(data.getWidth - 1) ^ u(data.getWidth - 2)
        }.foldLeft(false.B)(_ || _)
      case _ =>
        w_overflow := false.B
    }

    val butterfly_pass = Seq.fill(2)(Wire(params.dataType))
    butterfly_pass.head := w_butterfly.head
    butterfly_pass.last := w_butterfly.last

    val butterfly_div_2_scaled = Seq.fill(2)(Wire(params.dataType))
    butterfly_div_2_scaled.head := butterfly_div_2.head
    butterfly_div_2_scaled.last := butterfly_div_2.last

    w_butterfly_scaled.head := Mux(
      io.i_divBy2.getOrElse(params.divBy2.B),
      butterfly_div_2_scaled.head,
      butterfly_pass.head
    )
    w_butterfly_scaled.last := Mux(
      io.i_divBy2.getOrElse(params.divBy2.B),
      butterfly_div_2_scaled.last,
      butterfly_pass.last
    )
  }

  // Overflow register
  if (params.overflowReg) {
    io.o_overflow.get := w_overflow
  }
}
