package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

/**
 * This module generates two variants of the hardware:
 *
 * magType = LogSquaredJPL block diagram:
 *
 * {{{
 *                            +------------------+     +--------------+
 *            i_sel=1    +--> | MagnitudeSquared | --> | MagnitudeLog | --+     i_sel=1
 *           +-------+   |    +------------------+     +--------------+   |     +-----+
 *           |       | --+                                                + --> |     |
 * Input --> | DeMux |                                                          | Mux | --> Output
 *           |       | --+                                                + --> |     |
 *           +-------+   |             +--------------+                   |     +-----+
 *            i_sel=0    +-----------> | MagnitudeJPL | ------------------+     i_sel=0
 *                                     +--------------+
 * }}}
 *
 * * magType = LogJPLSquared block diagram:
 *
 * {{{
 *                                +--------------+     +--------------+
 *            i_sel=1    +------> | MagnitudeJPL | --> | MagnitudeLog | --+     i_sel=1
 *           +-------+   |        +--------------+     +--------------+   |     +-----+
 *           |       | --+                                                + --> |     |
 * Input --> | DeMux |                                                          | Mux | --> Output
 *           |       | --+                                                + --> |     |
 *           +-------+   |             +------------------+               |     +-----+
 *            i_sel=0    +-----------> | MagnitudeSquared | --------------+     i_sel=0
 *                                     +------------------+
 * }}}
 */
class MagnitudeMuxed[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  require(params.magType == LogJPLSquared || params.magType == LogSquaredJPL)

  private val squaredParameters = LogMagnitudeParams(
    inputType   = params.inputType,
    outputType  = if (params.magType == LogSquaredJPL) params.realType.get else params.outputType,
    magType     = Squared,
    addPipeRegs = params.addPipeRegs,
    mulPipeRegs = params.mulPipeRegs,
    trimType    = params.trimType
  )

  private val jplParameters = LogMagnitudeParams(
    inputType   = params.inputType,
    outputType  = if (params.magType == LogJPLSquared) params.realType.get else params.outputType,
    magType     = JPL,
    addPipeRegs = params.addPipeRegs,
    mulPipeRegs = params.mulPipeRegs,
    trimType    = params.trimType
  )

  private val logParameters = LogMagnitudeParams(
    inputType     = params.inputType,
    realType      = params.realType,
    outputType    = params.outputType,
    magType       = Log,
    lutTableSize  = params.lutTableSize,
    lutTableWidth = params.lutTableWidth,
    addPipeRegs   = params.addPipeRegs,
    mulPipeRegs   = params.mulPipeRegs,
    trimType      = params.trimType
  )

  // IO
  val io: LogMagnitudeIO[T] = IO(new LogMagnitudeIO(params))

  // Modules
  private val magSquared = Module(new MagnitudeSquared(squaredParameters))
  private val magJPL     = Module(new MagnitudeJPL(jplParameters))
  private val magLog     = Module(new MagnitudeLog(logParameters))

  if (params.magType == LogJPLSquared) {
    magLog.io.in <> magJPL.io.out
    magLog.io.i_last := magJPL.io.o_last
    when(io.i_sel.get) {
      io.out <> magLog.io.out
      io.o_last := magLog.io.o_last
      magJPL.io.in <> io.in
      magJPL.io.i_last := io.i_last
      // Magnitude Squared is not connected
      magSquared.io.in.valid  := false.B
      magSquared.io.i_last    := false.B
      magSquared.io.in.bits   := 0.U.asTypeOf(magSquared.io.in.bits)
      magSquared.io.out.ready := false.B
    }.otherwise {
      io.out <> magSquared.io.out
      io.o_last := magSquared.io.o_last
      magSquared.io.in <> io.in
      magSquared.io.i_last := io.i_last
      // Magnitude JPL is not connected
      magJPL.io.in.valid  := false.B
      magJPL.io.i_last    := false.B
      magJPL.io.in.bits   := 0.U.asTypeOf(magJPL.io.in.bits)
      magLog.io.out.ready := false.B
    }
  } else {
    magLog.io.in <> magSquared.io.out
    magLog.io.i_last <> magSquared.io.o_last
    when(io.i_sel.get) {
      io.out <> magLog.io.out
      io.o_last := magLog.io.o_last
      magSquared.io.in <> io.in
      magSquared.io.i_last := io.i_last
      // Magnitude JPL is not connected
      magJPL.io.in.valid  := false.B
      magJPL.io.i_last    := false.B
      magJPL.io.in.bits   := 0.U.asTypeOf(magJPL.io.in.bits)
      magJPL.io.out.ready := false.B
    }.otherwise {
      io.out <> magJPL.io.out
      io.o_last := magJPL.io.o_last
      magJPL.io.in <> io.in
      magJPL.io.i_last := io.i_last
      // Magnitude Squared is not connected
      magSquared.io.in.valid := false.B
      magSquared.io.i_last   := false.B
      magSquared.io.in.bits  := 0.U.asTypeOf(magSquared.io.in.bits)
      magLog.io.out.ready    := false.B
    }
  }
}

object MagnitudeMuxedApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType     = DspComplex(FixedPoint(16.W, 14.BP)),
    realType      = Some(FixedPoint(19.W, 14.BP)),
    outputType    = FixedPoint(25.W, 14.BP),
    magType       = LogJPLSquared,
    lutTableSize  = Some(10),
    lutTableWidth = Some(12),
    addPipeRegs   = false,
    mulPipeRegs   = false,
    trimType      = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeMuxed(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeMuxed"))
  )
}
