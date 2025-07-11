package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

class MagnitudeMuxed[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  require(params.magType == LogJPLSquared || params.magType == LogSquaredJPL)

  private val squaredParameters = LogMagnitudeParams(
    inputType    = params.inputType,
    realType     = None,
    outputType   = if (params.magType == LogSquaredJPL) params.realType.get else params.outputType,
    logType      = None,
    magType      = Squared,
    addPipeRegs  = params.addPipeRegs,
    mulPipeRegs  = params.mulPipeRegs,
    binaryGrowth = params.binaryGrowth,
    trimType     = params.trimType
  )

  private val jplParameters = LogMagnitudeParams(
    inputType    = params.inputType,
    realType     = None,
    outputType   = if (params.magType == LogJPLSquared) params.realType.get else params.outputType,
    logType      = None,
    magType      = JPL,
    addPipeRegs  = params.addPipeRegs,
    mulPipeRegs  = params.mulPipeRegs,
    binaryGrowth = params.binaryGrowth,
    trimType     = params.trimType
  )

  private val logParameters = LogMagnitudeParams(
    inputType    = params.inputType,
    realType     = params.realType,
    outputType   = params.outputType,
    logType      = params.logType,
    magType      = Log,
    lutTableSize = params.lutTableSize,
    addPipeRegs  = params.addPipeRegs,
    mulPipeRegs  = params.mulPipeRegs,
    binaryGrowth = params.binaryGrowth,
    trimType     = params.trimType
  )

  // IO
  val io: LogMagnitudeIO[T] = IO(new LogMagnitudeIO(params))

  // Modules
  private val magSquared = Module(new MagnitudeSquared(squaredParameters))
  private val magJPL     = Module(new MagnitudeJPL(jplParameters))
  private val magLog     = Module(new MagnitudeLog(logParameters))

  if (params.magType == LogJPLSquared) {
    magLog.io.in <> magJPL.io.out
    when(io.sel.get) {
      io.out <> magLog.io.out
      magJPL.io.in <> io.in
      magSquared.io.in.valid  := false.B
      magSquared.io.in.bits   := 0.U.asTypeOf(magSquared.io.in.bits)
      magSquared.io.out.ready := false.B
    }.otherwise {
      io.out <> magSquared.io.out
      magSquared.io.in <> io.in
      magJPL.io.in.valid  := false.B
      magJPL.io.in.bits   := 0.U.asTypeOf(magJPL.io.in.bits)
      magLog.io.out.ready := false.B
    }
  } else {
    magLog.io.in <> magSquared.io.out
    when(io.sel.get) {
      io.out <> magLog.io.out
      magSquared.io.in <> io.in
      magJPL.io.in.valid  := false.B
      magJPL.io.in.bits   := 0.U.asTypeOf(magJPL.io.in.bits)
      magJPL.io.out.ready := false.B
    }.otherwise {
      io.out <> magJPL.io.out
      magJPL.io.in <> io.in
      magSquared.io.in.valid := false.B
      magSquared.io.in.bits  := 0.U.asTypeOf(magSquared.io.in.bits)
      magLog.io.out.ready    := false.B
    }
  }

}


object MagnitudeMuxedApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    realType     = Some(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(16.W, 14.BP),
    logType      = Some(FixedPoint(16.W, 14.BP)),
    magType      = LogJPLSquared,
    addPipeRegs  = false,
    mulPipeRegs  = false,
    binaryGrowth = 0,
    trimType     = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeMuxed(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeMuxed"))
  )
}
