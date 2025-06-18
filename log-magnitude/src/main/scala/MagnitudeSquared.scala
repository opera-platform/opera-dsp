package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

class MagnitudeSquared[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  // IO
  val io = IO(new LogMagnitudeIO(params))

  // Get I (real) and Q (imaginary) absolute values
  val absI = Real[T].abs(io.in.bits.real)
  val absQ = Real[T].abs(io.in.bits.imag)

  val A: T = DspContext.alter(
    DspContext.current
      .copy(numAddPipes = params.addPipeRegs, numMulPipes = params.mulPipeRegs, binaryPointGrowth = params.binaryGrowth)
  ) {
      (absI.context_*(absI)).context_+(absQ.context_*(absQ))
  }

  val w_out = Wire(io.out.cloneType)
  w_out.bits := A
  w_out.valid := io.in.valid
  io.in.ready := w_out.ready
  AlignHandshake(2 * params.addPipeRegs, w_out, io.out) := A

}


object MagnitudeSquaredApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(16.W, 14.BP),
    magType      = Squared,
    addPipeRegs  = 1,
    mulPipeRegs  = 1,
    binaryGrowth = 0,
    trimType     = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeSquared(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeSquared"))
  )
}
