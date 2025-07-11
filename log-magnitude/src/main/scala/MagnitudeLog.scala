package opera.logmagnitude

import breeze.linalg.max
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint, fromSIntToFixedPoint}

// out = log(A)
// N = 2^k * (1 + f)
// log(N) = k + log2(1 + f)
class MagnitudeLog[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  val addPipeRegs: Int = if (params.addPipeRegs) 1 else 0

  // IO
  val io: LogIO[T] = IO(new LogIO(params))

  private val A = io.in.bits

  private val inputWidth  = params.realType.get.getWidth
  private val logWidth    = params.logType.get.getWidth
  private val outputWidth = params.outputType.getWidth

  private val inputBinPointPosition = params.realType.get match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }
  private val logBinPointPosition = params.logType.get match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }
  private val outputBinPointPosition = params.outputType match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  }

  // Generate Look Up Table, LUT contains value log2(1 + f)
  private val logLUT = VecInit({
    val lnOf2 = scala.math.log(2) // natural log of 2
    def log2(x: Double): Double = scala.math.log(x) / lnOf2

    val sizeLUT = 1 << params.lutTableSize
    val LUT = (0 until sizeLUT).map(n => {
      val lookupWire = Wire(FixedPoint(logWidth.W, logBinPointPosition.BP))
      // log2(1 + f)
      DspContext.withTrimType(Convergent) {
        lookupWire := lookupWire.cloneType.fromDoubleWithFixedWidth(log2(1 + n.toDouble / sizeLUT))
      }
      lookupWire
    })
    LUT
  })

  // Find the location of most significant bit that is equal to one
  private val log2A = Log2(A.asUInt)
  // Calculate k
  private val k = Wire(SInt((inputWidth - inputBinPointPosition).W))
  k :=
    (log2A.asTypeOf(UInt((inputWidth - inputBinPointPosition).W)) -
    inputBinPointPosition.U.asTypeOf(UInt((inputWidth - inputBinPointPosition).W))).asTypeOf(k)

  // Calculate LUT address
  private val address =
    BinaryRepresentation[UInt].shr(Cat(A.asUInt, 0.U(params.lutTableSize.W)), log2A)(params.lutTableSize - 1, 0)

  // Get LUT value
  private val logFraction = Wire(FixedPoint(logWidth.W, logBinPointPosition.BP))
  logFraction := logLUT(address)

  // out = k + logFraction
  private val log2MagBinPoint: Int = max(outputBinPointPosition, logBinPointPosition)
  private val log2MagWidth: Int = outputWidth - outputBinPointPosition + log2MagBinPoint
  private val log2Mag = Wire(FixedPoint(log2MagWidth.W, log2MagBinPoint.BP))
  log2Mag := k.asFixedPoint(0.BP)  + logFraction
  // Optional pipe register
  private val r_log2Mag = if (params.addPipeRegs) Some(Reg(log2Mag.cloneType)) else None

  private val output =
    if (outputBinPointPosition > logBinPointPosition)
      if (params.addPipeRegs) r_log2Mag.get else log2Mag
    else {
      DspContext.alter(DspContext.current.copy(
        binaryPointGrowth = 0, trimType = params.trimType
      )) {
        if (params.addPipeRegs)
          r_log2Mag.get.div2(logBinPointPosition - outputBinPointPosition)
        else
          log2Mag.div2(logBinPointPosition - outputBinPointPosition)
      }
    }

  io.out.bits  := output.asTypeOf(io.out.bits)

  // Handshake control
  if (params.addPipeRegs) {
    val r_last    = Reg(Vec(addPipeRegs, Bool()))
    val handshake = AlignHandshake(addPipeRegs, io.in.valid, io.out.ready)

    when(handshake._1.head) {
      r_last.head   := io.i_last
      r_log2Mag.get := log2Mag
    }
    io.o_last    := r_last.last
    io.in.ready  := handshake._1.head
    io.out.valid := handshake._2.head
  }
  else {
    io.out.valid := io.in.valid
    io.in.ready  := io.out.ready
    io.o_last    := io.i_last
  }
}


object MagnitudeLogApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(20.W, 14.BP)),
    realType     = Some(FixedPoint(20.W, 14.BP)),
    outputType   = FixedPoint(20.W, 14.BP),
    magType      = Log,
    logType      = Some(FixedPoint(16.W, 14.BP)),
    lutTableSize = 10,
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
