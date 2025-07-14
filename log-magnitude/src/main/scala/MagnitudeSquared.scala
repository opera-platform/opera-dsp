package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

/**
 * Computes the magnitude squared.
 *
 * The calculation is defined as:
 *    A = I * I + Q * Q
 */
class MagnitudeSquared[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  val addPipeRegs: Int = if (params.addPipeRegs) 1 else 0
  val mulPipeRegs: Int = if (params.mulPipeRegs) 1 else 0

  // Data widths
  val inputWidth : Int = params.inputType.getWidth / 2
  val outputWidth: Int = params.outputType.getWidth
  // Data binary points
  val inputBinPoint = params.inputType.real match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }
  val outputBinPoint = params.outputType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }
  // Requirement for correct result
  require((outputWidth - outputBinPoint) >= (2*(inputWidth - inputBinPoint) + 1))

  // IO
  val io: LogMagnitudeIO[T] = IO(new LogMagnitudeIO(params))

  // Get I (real) and Q (imaginary) absolute values
  private val I: SInt = io.in.bits.real.asTypeOf(SInt(inputWidth.W))
  private val Q: SInt = io.in.bits.imag.asTypeOf(SInt(inputWidth.W))

  // I*I
  private val squareI = (I * I).asTypeOf(FixedPoint((2*inputWidth).W, (2*inputBinPoint).BP))
  private val r_squareI = if (params.mulPipeRegs) Some(Reg(squareI.cloneType)) else None

  // Q*Q
  private val squareQ = (Q * Q).asTypeOf(FixedPoint((2*inputWidth).W, (2*inputBinPoint).BP))
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

  private val A = if (params.addPipeRegs) r_sumSquares.get else sumSquares

  // Calculate the number of bits that needs to be trimmed
  private val trimBits = if ((A.binaryPoint.get - outputBinPoint) > 0) A.binaryPoint.get - outputBinPoint else 0

  private val trimA = DspContext.alter(DspContext.current.copy(
    binaryPointGrowth = 0, trimType = params.trimType
  )) {
    A.div2(trimBits)
  }

  io.out.bits := trimA.asTypeOf(io.out.bits)

  // Handshake control
  if (params.addPipeRegs || params.mulPipeRegs) {
    val r_last    = Reg(Vec(addPipeRegs + mulPipeRegs, Bool()))
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
          r_last(i) := io.i_last
        }
        else {
          r_sumSquares.get := sumSquares
          r_last(i) := r_last(i-1)
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


object MagnitudeSquaredApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(20.W, 14.BP),
    magType      = Squared,
    addPipeRegs  = true,
    mulPipeRegs  = true,
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
