package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

// TODO: Add delays, we have pipeline registers but not alignment for ready-valid
// Jet Propulsion Laboratory magnitude approximation
//     { 1.0 * X + 1/8 * Y;  X >= 3Y
// A = {
//     { 7/8 * X + 1/2 * Y;  X <= 3Y
// X = max(|I|,|Q|)
// Y = min(|I|,|Q|)
// Paper: https://ipnpr.jpl.nasa.gov/progress_report/42-40/40L.PDF
class MagnitudeJPL[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  // IO
  val io = IO(new LogMagnitudeIO(params))

  // Get I (real) and Q (imaginary) absolute values
  val absI = Real[T].abs(io.in.bits.real)
  val absQ = Real[T].abs(io.in.bits.imag)
  // Calculate X and Y
  val x = Real[T].max(absI, absQ)
  val y = Real[T].min(absI, absQ)

  // A = 1.0 * X + 1/8 * Y;  X >= 3Y
  // Align geA (greater or equal A) with leA (less or equal A)
  val geA = DspContext.withNumAddPipes(2 * params.addPipeRegs) {
    x.context_+(BinaryRepresentation[T].shr(y, 3))
  }

  // We want to avoid multiplication 7/8 * X. So we will instead subtract 1/8*X from X
  private val x_7_8 = DspContext.withNumAddPipes(params.addPipeRegs) { x.context_-(BinaryRepresentation[T].shr(x, 3)) }
  // A= 7/8 * X + 1/2 * Y;  X <= 3Y
  val leA = DspContext.withNumAddPipes(params.addPipeRegs) {
    x_7_8.context_+(ShiftRegister(BinaryRepresentation[T].shr(y, 1), params.addPipeRegs, true.B))
  } // (7/8)*U + 1/2*V
  private val A = Real[T].max(geA, leA)

  // Connect output
  io.out.bits  := A
  io.out.valid := io.in.valid
  io.in.ready  := io.out.ready

  val w_out = Wire(io.out.cloneType)
  w_out.bits  := A
  w_out.valid := io.in.valid
  io.in.ready := w_out.ready
  AlignHandshake(2*params.addPipeRegs, w_out, io.out) := A
}


object MagnitudeJPLApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(16.W, 14.BP),
    magType      = JPL,
    addPipeRegs  = 1,
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
