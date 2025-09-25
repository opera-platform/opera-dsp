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
  print(f"delay: ${params.delay}, latency: ${params.latency}\n")
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
  private val r_counter = RegInit(0.U(log2Ceil(params.fftSize).W))
  private val r_counter_2 = RegInit(((params.fftSize/2 + 1) & (params.fftSize - 1)).U(log2Ceil(params.fftSize).W))
  private val w_en_delayed = ShiftRegister(io.i_en, latency, true.B)


  r_counter := r_counter + io.i_en
  r_counter_2 := r_counter_2 + io.i_en
  dontTouch(r_counter)
  dontTouch(r_counter_2)

  print(f"decimation: ${params.decimation}, stage size: ${params.fftSize}, log ${log2Ceil(params.fftSize)}\n")
  dontTouch(w_delay_mux_ctrl)
  dontTouch(w_output_mux_ctrl)
  w_delay_mux_ctrl.suggestName("w_delay_mux_ctrl")
  w_output_mux_ctrl.suggestName("w_output_mux_ctrl")
  w_delay_in := Mux(w_delay_mux_ctrl, io.in, w_butterfly_scaled(1))
  if (params.decimation == DIF) {
    io.o_counter       := r_counter
    w_delay_mux_ctrl  := r_counter < delay.U
    w_delay_out       := DelayBuffer(w_delay_in, delay, io.i_en, params.singlePortMem, params.bufferAsMem)
    w_output_mux_ctrl := ShiftRegister(w_delay_mux_ctrl, params.addPipeRegs, false.B, true.B)
  } else {
    io.o_counter      := r_counter_2
    w_delay_mux_ctrl  := ShiftRegister(r_counter_2 < delay, latency, false.B, true.B)
    w_delay_out       := DelayBuffer(w_delay_in, delay, w_en_delayed, params.singlePortMem, params.bufferAsMem)
    w_output_mux_ctrl := ShiftRegister(r_counter_2 < delay.U, params.addPipeRegs + latency, false.B, true.B)
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

    params.dataType.real match {
      case fp: FixedPoint =>
        w_overflow := Seq(w_butterfly.head.real, w_butterfly.head.imag, w_butterfly.last.real, w_butterfly.last.imag).map(sGrow => {
          val width = sGrow.getWidth
          val binaryPoint = fp.binaryPoint.get
          val tooBig = !sGrow.isSignNegative && (BinaryRepresentation[T].shr(sGrow, width - 2) === Real[T]
            .fromDouble(1 / math.pow(2, binaryPoint)))
          val tooSmall =
            sGrow.isSignNegative && (BinaryRepresentation[T].shr(sGrow, width - 2) === Real[T].fromDouble(0.0))
          tooBig || tooSmall
        }).foldLeft(false.B)(_ || _)
      case _ =>
        w_overflow := false.B
    }

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

  io.o_en := ShiftRegisterWithReset(io.i_en, latency + params.addPipeRegs, false.B, reset.asBool, true.B)
  dontTouch(io.o_en)
}
