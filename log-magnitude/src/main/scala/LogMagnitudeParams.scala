package opera.logmagnitude

import chisel3.{Data, fromIntToWidth}
import dsptools.TrimType
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._

sealed trait MagType

case object JPL extends MagType
case object Log extends MagType
case object Squared extends MagType
case object LogJPLSquared extends MagType
case object LogSquaredJPL extends MagType

/**
 * Parameters for configuring the LogMagnitude hardware module.
 *
 * @param inputType     Input DspComplex[T] data type (not used in MagnitudeLog).
 * @param realType      Optional real data type (only relevant for MagnitudeLog implementations).
 * @param outputType    Output data type.
 * @param magType       Selection of magnitude approximation type (default: `JPL`).
 * @param lutTableSize  Size of the LUT, where the actual table size is `2^{lutTableSize}` (only used for MagnitudeLog).
 * @param lutTableWidth Optional Look-Up Table (LUT) data width (only used if MagnitudeLog is generated).
 * @param addPipeRegs   If `true`, inserts pipeline registers after addition stages.
 * @param mulPipeRegs   If `true`, inserts pipeline registers after multiplication stages.
 * @param trimType      Specifies the trimming strategy applied after arithmetic operations.
 */
case class LogMagnitudeParams[T <: Data](
  inputType     : DspComplex[T],
  realType      : Option[T] = None,
  outputType    : T,
  magType       : MagType = JPL,
  lutTableSize  : Option[Int] = None,
  lutTableWidth : Option[Int] = None,
  addPipeRegs   : Boolean = false,
  mulPipeRegs   : Boolean = false,
  trimType      : TrimType
)

object LogMagnitudeParams {
  def fixed (
    inputType: DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
    realType: Option[FixedPoint] = None,
    outputType: FixedPoint = FixedPoint(18.W, 14.BP),
    magType: MagType = JPL,
    lutTableSize: Option[Int] = None,
    lutTableWidth: Option[Int] = None,
    addPipeRegs: Boolean = false,
    mulPipeRegs: Boolean = false,
    trimType: TrimType = Convergent
  ): LogMagnitudeParams[FixedPoint] = {
    LogMagnitudeParams(
      inputType     = inputType,
      realType      = realType,
      outputType    = outputType,
      lutTableSize  = lutTableSize,
      lutTableWidth = lutTableWidth,
      magType       = magType,
      addPipeRegs   = addPipeRegs,
      mulPipeRegs   = mulPipeRegs,
      trimType      = trimType
    )
  }
}