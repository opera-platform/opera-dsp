package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint, fromSIntToFixedPoint}

// TODO: Add delays, we have pipeline registers but not alignment for ready-valid
// Jet Propulsion Laboratory magnitude approximation
//     { 1.0 * X + 1/8 * Y;  X >= 3Y
// A = {
//     { 7/8 * X + 1/2 * Y;  X <= 3Y
// X = max(|I|,|Q|)
// Y = min(|I|,|Q|)
// out = log(A)
// Paper: https://ipnpr.jpl.nasa.gov/progress_report/42-40/40L.PDF
class MagnitudeLogJPL[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
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
  }
  private val A = Real[T].max(geA, leA)
  
  private val latencyJPL = 2 * params.addPipeRegs
  val latency: Int = latencyJPL + params.addPipeRegs
  
  private val inputWidth = params.inputType.getWidth
  private val logWidth   = params.logType.get.getWidth
  
  private val inputBinPointPosition = params.inputType match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }
  private val logBinPointPosition = params.logType.get match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }
  
  private val logUInt = Log2(A.asUInt)
  private val logSInt = (logUInt - inputBinPointPosition.U).asSInt

  // log(N) = k + log2(1 + f)
  // N = 2^k(1 + f)
  private val logLUT = VecInit({
    val lnOf2 = scala.math.log(2) // natural log of 2

    def log2(x: Double): Double = scala.math.log(x) / lnOf2

    val sizeLUT = 1 << params.lutDataWidth
    val LUT = (0 until sizeLUT).map(n => {
      val lookupWire = Wire(FixedPoint((logBinPointPosition + 1).W, logBinPointPosition.BP))
      // log2(1+f)
      DspContext.withTrimType(Convergent) {
        lookupWire := lookupWire.cloneType.fromDoubleWithFixedWidth(log2(n.toDouble / sizeLUT + 1))
      }
      lookupWire
    })
    LUT
  })
  
  private val noLeadOne = ShiftRegister(
    A.asUInt - BinaryRepresentation[UInt].shl(1.U, logUInt).asTypeOf(A.asUInt),
    params.addPipeRegs,
    true.B
  )
  private val shiftNum = ShiftRegister(inputWidth.U - logUInt - 1.U, params.addPipeRegs, true.B)

  private val logLUTAddress =
    BinaryRepresentation[UInt].shl(noLeadOne, shiftNum)(inputWidth - 2, inputWidth - params.lutDataWidth - 1)
  private val logFraction = Wire(FixedPoint((logBinPointPosition + 1).W, logBinPointPosition.BP))

  logFraction := logLUT(logLUTAddress)

  private val log2Mag = Wire(FixedPoint(logWidth.W, logBinPointPosition.BP))
  log2Mag := ShiftRegister(logSInt.asFixedPoint(0.BP), params.addPipeRegs, true.B) + logFraction

  private val output = Wire(params.outputType.cloneType)
  output := log2Mag

  val w_out: io.out.type = Wire(io.out.cloneType)
  w_out.bits := output
  w_out.valid := io.in.valid
  io.in.ready := w_out.ready
  AlignHandshake(latency, w_out, io.out) := log2Mag
}


object MagnitudeLogJPLApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(16.W, 14.BP),
    magType      = LogJPL,
    logType      = Some(FixedPoint(16.W, 14.BP)),
    lutDataWidth = 16,
    addPipeRegs  = 1,
    binaryGrowth = 0,
    trimType     = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeLogJPL(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeLogJPL"))
  )
}
