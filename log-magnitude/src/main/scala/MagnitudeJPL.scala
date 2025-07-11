package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

// Jet Propulsion Laboratory magnitude approximation
//     { 1.0 * X + 1/8 * Y;  X >= 3Y
// A = {
//     { 7/8 * X + 1/2 * Y;  X <= 3Y
// X = max(|I|,|Q|)
// Y = min(|I|,|Q|)
// Paper: https://ipnpr.jpl.nasa.gov/progress_report/42-40/40L.PDF
class MagnitudeJPL[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  val addPipeRegs: Int = if (params.addPipeRegs) 1 else 0

  // IO
  val io: LogMagnitudeIO[T] = IO(new LogMagnitudeIO(params))

  // Get I (real) and Q (imaginary) absolute values
  val absI: T = Real[T].abs(io.in.bits.real)
  val absQ: T = Real[T].abs(io.in.bits.imag)
  // Calculate X and Y
  val x: T = Real[T].max(absI, absQ)
  val y: T = Real[T].min(absI, absQ)

  // A = 1.0 * X + 1/8 * Y;  X >= 3Y
  // Align geA (greater or equal A) with leA (less or equal A)
  val geA: T = DspContext.alter(DspContext.current.copy(
      trimType = params.trimType
    )) {
    x.context_+(BinaryRepresentation[T].shr(y, 3))
  }
  private val r_geA: Option[Vec[geA.type]] = if (params.addPipeRegs) Some(Reg(Vec(2 * addPipeRegs, geA.cloneType))) else None

  // We want to avoid multiplication 7/8 * X. So we will instead subtract 1/8*X from X
  private val x_7_8 = DspContext.alter(DspContext.current.copy(
    trimType = params.trimType
  )) {
    x.context_-(BinaryRepresentation[T].shr(x, 3))
  }
  private val r_x_7_8: Option[Vec[x_7_8.type]] = if (params.addPipeRegs) Some(Reg(Vec(addPipeRegs, x_7_8.cloneType))) else None


  // A= 7/8 * X + 1/2 * Y;  X <= 3Y
  val leA: T = DspContext.alter(DspContext.current.copy(
    trimType = params.trimType
  )) {
    if (params.addPipeRegs)
      r_x_7_8.get.head.context_+(ShiftRegister(BinaryRepresentation[T].shr(y, 1), addPipeRegs, true.B))
    else
      x_7_8.context_+(ShiftRegister(BinaryRepresentation[T].shr(y, 1), addPipeRegs, true.B))
  }
  private val r_leA: Option[Vec[leA.type]] = if (params.addPipeRegs) Some(Reg(Vec(addPipeRegs, leA.cloneType))) else None

  private val A =
    if (params.addPipeRegs)
      Real[T].max(r_geA.get.last, r_leA.get.last)
    else
      Real[T].max(geA, leA)

  io.out.bits := A.asTypeOf(io.out.bits)

  // Handshake control
  if (params.addPipeRegs) {
    val r_last    = Reg(Vec(2 * addPipeRegs, Bool()))
    val handshake = AlignHandshake(2 * addPipeRegs, io.in.valid, io.out.ready)

    for (i <- 0 until 2 * addPipeRegs) {
      when(handshake._1(i)) {
        if (i == 0) {
          r_last(i)        := io.i_last
          r_geA.get(i)     := geA
          r_x_7_8.get.head := x_7_8
        }
        else {
          r_last(i)      := r_last(i-1)
          r_geA.get(i)   := r_geA.get(i-1)
          r_leA.get.head := leA
        }
      }
    }
    io.o_last    := r_last.last
    io.in.ready  := handshake._1.head
    io.out.valid := handshake._2.last
  }
  else {
    io.out.valid := io.in.valid
    io.in.ready  := io.out.ready
    io.o_last    := io.i_last
  }
}


object MagnitudeJPLApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(16.W, 14.BP),
    magType      = JPL,
    addPipeRegs  = false,
    binaryGrowth = 0,
    trimType     = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeJPL(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeJPL"))
  )
}
