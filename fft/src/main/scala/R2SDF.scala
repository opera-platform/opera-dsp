package opera.fft

import breeze.numerics.constants.Pi
import breeze.numerics.{cos, sin}
import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

class R2SDF[T <: Data: Real: BinaryRepresentation] (params: RadixParams[T]) extends Module {

  // Variables
  private val latency     = params.latency
  private val delay       = params.delay
  private val binPosition = params.dataType.real match {
    case data: FixedPoint => data.binaryPoint.get
    case _                => 0
  }

  // IOs
  val io: RadixIO[T] = IO(new RadixIO(params))
  // Wires
  private val w_in               = Wire(params.dataType)
  private val w_out              = Wire(params.dataType)
  private val w_delay_mux_ctrl   = Wire(Bool())
  private val w_delay_in         = Wire(params.dataType)
  private val w_delay_out        = Wire(params.dataType)
  private val w_butterfly        = Butterfly[T](Seq(w_delay_out, w_in))
  private val w_butterfly_scaled = Seq.fill(2)(Wire(params.dataType))
  private val w_output_mux_ctrl  = Wire(Bool())
  private val w_overflow         = Wire(Bool())
  // Twiddle factor
  private val w_twiddle = Wire(params.twiddleType)
  private val m_twiddle = Wire(Vec(delay, params.twiddleType)) // ROM
  // Generate twiddle ROM data. TODO: Maybe reduce the memory requirement
  m_twiddle.zipWithIndex.foreach { case (twiddle, k) =>
    DspContext.withTrimType(Convergent) {
        twiddle.real := params.twiddleType.real.fromDoubleWithFixedWidth( cos(2.0*Pi*k/(2*delay)))
        twiddle.imag := params.twiddleType.real.fromDoubleWithFixedWidth(-sin(2.0*Pi*k/(2*delay)))
    }
  }
  
  w_delay_in := Mux(w_delay_mux_ctrl, w_in, w_butterfly_scaled.last)
  if (params.decimation == DIF) {
    w_twiddle         := ShiftRegister(m_twiddle(io.i_counter(log2Ceil(delay)-1, 0)), params.addPipeRegs, true.B)
    w_delay_mux_ctrl  := io.i_counter < delay.U
    w_delay_out       := DelayBuffer(w_delay_in, delay, io.i_en, params.singlePortMem, params.bufferAsMem)
    w_output_mux_ctrl := ShiftRegister(w_delay_mux_ctrl, params.addPipeRegs, false.B, true.B)
    // Twiddle factor at the output
    w_in := io.in
    when(ShiftRegister(io.i_counter < delay.U && io.i_counter =/= 0.U, latency + params.addPipeRegs, true.B)) {
      io.out := DspContext.alter(
        DspContext.current.copy(trimType = params.trimType, numAddPipes = params.addPipeRegs, numMulPipes = params.mulPipeRegs)) {
          DspContext.alter(DspContext.current.copy(trimType = NoTrim, overflowType = Grow, complexUse4Muls = params.dspMul4)) {
            w_out.context_*(w_twiddle)
          }.trimBinary(binPosition)
      }
    }.otherwise {
      io.out := ShiftRegister(w_out, latency, true.B)
    }
  } else {
    w_twiddle         := m_twiddle(io.i_counter(log2Ceil(delay)-1, 0))
    w_delay_mux_ctrl  := ShiftRegister(io.i_counter < delay.U, latency, false.B, true.B)
    w_delay_out       := DelayBuffer(w_delay_in, delay, ShiftRegister(io.i_en, latency, true.B), params.singlePortMem, params.bufferAsMem)
    w_output_mux_ctrl := ShiftRegister(io.i_counter < delay.U, params.addPipeRegs + latency, false.B, true.B)
    // Twiddle factor at the input
    when(ShiftRegister(io.i_counter > delay, latency, true.B)) {
      w_in := DspContext.alter(
        DspContext.current.copy(trimType = params.trimType, numAddPipes = params.addPipeRegs, numMulPipes = params.mulPipeRegs)) {
          DspContext.alter(DspContext.current.copy(trimType = NoTrim, overflowType = Grow, complexUse4Muls = params.dspMul4)) {
            io.in.context_*(w_twiddle)
          }.trimBinary(binPosition)
        }
    }.otherwise {
      w_in := ShiftRegister(io.in, latency, true.B)
    }
    io.out := w_out
  }
  // Output control
  w_out := Mux(
    w_output_mux_ctrl,
    ShiftRegister(w_delay_out, params.addPipeRegs, true.B),
    ShiftRegister(w_butterfly_scaled.head, params.addPipeRegs, true.B)
  )
  // Grow logic
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

    //TODO: overflow

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
