package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint, fromSIntToFixedPoint}

// out = log(A)
class MagnitudeLog[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  // IO
  val io: LogIO[T] = IO(new LogIO(params))

  private val A = io.in.bits

  val latency: Int = if (params.addPipeRegs) 1 else 0

  private val inputWidth = params.realType.get.getWidth

  private val inputBinPointPosition = params.realType.get match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }
  private val logBinPointPosition = params.logType.get match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }

  // Generate Look Up Table
  // N = 2^k * (1 + f)
  // log(N) = k + log2(1 + f)
  // LUT contains: log2(1 + f)
  private val logLUT = VecInit({
    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2

    val sizeLUT = 1 << params.lutTableSize
    val LUT = (0 until sizeLUT).map(n => {
      val lookupWire = Wire(FixedPoint((logBinPointPosition + 1).W, logBinPointPosition.BP))
      // log2(1 + f)
      DspContext.withTrimType(Convergent) {
        lookupWire := lookupWire.cloneType.fromDoubleWithFixedWidth(log2(1 + n.toDouble / sizeLUT))
      }
      lookupWire
    })
    LUT
  })

  // Calculate k
  private val log2A = Log2(A.asUInt) // Find the location of most significant bit that is equal to one
  dontTouch(log2A)
  log2A.suggestName("log2A")
  private val k = Wire(SInt((inputWidth - inputBinPointPosition).W))
  k :=
    (log2A.asTypeOf(UInt((inputWidth - inputBinPointPosition).W)) -
    inputBinPointPosition.U.asTypeOf(UInt((inputWidth - inputBinPointPosition).W))).asTypeOf(k)
  dontTouch(k)
  k.suggestName("k")

  // Calculate LUT address
  private val noLeadOne =  A.asUInt - BinaryRepresentation[UInt].shl(1.U, log2A).asTypeOf(A.asUInt)
  dontTouch(noLeadOne)
  noLeadOne.suggestName("noLeadOne")
  private val shiftNum = (inputWidth - 1).U - log2A
  dontTouch(shiftNum)
  shiftNum.suggestName("shiftNum")

  private val address = BinaryRepresentation[UInt].shl(noLeadOne, shiftNum)(inputWidth - 2, inputWidth - params.lutTableSize - 1)
  dontTouch(address)
  address.suggestName("address")

  // Get LUT value
  private val logFraction = Wire(FixedPoint((logBinPointPosition + 1).W, logBinPointPosition.BP))
  logFraction := logLUT(address)
  dontTouch(logFraction)
  logFraction.suggestName("logFraction")

  // out = k + logFraction
  private val log2Mag = Wire(params.outputType) // TODO: bilo je logType
  log2Mag := k.asFixedPoint(0.BP)  + logFraction
  dontTouch(log2Mag)
  log2Mag.suggestName("log2Mag")

  private val output = Wire(params.outputType.cloneType)
  output := log2Mag

  io.out.bits  := output
  io.out.valid := io.in.valid
  io.in.ready  := io.out.ready
}


object MagnitudeLogApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(20.W, 14.BP)),
    realType     = Some(FixedPoint(20.W, 14.BP)),
    outputType   = FixedPoint(20.W, 14.BP),
    magType      = Log,
    logType      = Some(FixedPoint(15.W, 14.BP)),
    lutTableSize = 4,
    addPipeRegs  = false,
    binaryGrowth = 0,
    trimType     = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeLog(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeLog"))
  )
}
