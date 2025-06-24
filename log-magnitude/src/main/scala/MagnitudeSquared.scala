package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

class MagnitudeSquared[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  val addPipeRegs: Int = if (params.addPipeRegs) 1 else 0
  val mulPipeRegs: Int = if (params.mulPipeRegs) 1 else 0

  // IO
  val io: LogMagnitudeIO[T] = IO(new LogMagnitudeIO(params))

  // Get I (real) and Q (imaginary) absolute values
  val absI: T = Real[T].abs(io.in.bits.real)
  val absQ: T = Real[T].abs(io.in.bits.imag)

  // I*I
  private val squareI = DspContext.alter(DspContext.current.copy(
    binaryPointGrowth = params.binaryGrowth, trimType = params.trimType
  )) {
    absI.context_*(absI)
  }
  private val r_squareI = if (params.mulPipeRegs) Some(Reg(squareI.cloneType)) else None

  // Q*Q
  private val squareQ = DspContext.alter(DspContext.current.copy(
    binaryPointGrowth = params.binaryGrowth, trimType = params.trimType
  )) {
    absQ.context_*(absQ)
  }
  private val r_squareQ = if (params.mulPipeRegs) Some(Reg(squareQ.cloneType)) else None

  // I*I + Q*Q
  private val sumSquares = DspContext.alter(DspContext.current.copy(
    trimType = params.trimType
  )) {
      if (params.mulPipeRegs)
        r_squareI.get.context_+(r_squareQ.get)
      else
        squareI.context_+(squareQ)
  }
  private val r_sumSquares = if (params.addPipeRegs) Some(Reg(sumSquares.cloneType)) else None

  private val A =
    if (params.addPipeRegs)
      r_sumSquares.get
    else
      sumSquares

  // Calculate the number of bits that needs to be trimmed
  private val trimBits = if ((A.getWidth - params.outputType.getWidth) > 0) A.getWidth - params.outputType.getWidth else 0

  private val trimA = DspContext.alter(DspContext.current.copy(
    binaryPointGrowth = 0, trimType = params.trimType
  )) {
    A.div2(trimBits)
  }

  io.out.bits := trimA.asTypeOf(io.out.bits)

  // Handshake control
  if (params.addPipeRegs || params.mulPipeRegs) {
    val handshake = AlignHandshake(addPipeRegs + mulPipeRegs, io.in.valid, io.out.ready)
    for (i <- 0 until addPipeRegs + mulPipeRegs) {
      when(handshake._1(i)) {
        if (i == 0) {
          if (params.mulPipeRegs) {
            r_squareI.get := squareI
            r_squareQ.get := squareQ
          }
          else {
            r_sumSquares.get := sumSquares
          }
        }
        else {
          r_sumSquares.get := sumSquares
        }
      }
    }
    io.in.ready := handshake._1.head
    io.out.valid := handshake._2.last
  }
  else {
    io.out.valid := io.in.valid
    io.in.ready := io.out.ready
  }

}


object MagnitudeSquaredApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(16.W, 14.BP),
    magType      = Squared,
    addPipeRegs  = true,
    mulPipeRegs  = true,
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
