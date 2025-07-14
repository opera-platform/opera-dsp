package opera.logmagnitude

import chisel3.Data
import dsptools.TrimType
import dsptools.numbers.DspComplex

sealed trait MagType

case object JPL extends MagType
case object Log extends MagType
case object Squared extends MagType
case object LogJPLSquared extends MagType
case object LogSquaredJPL extends MagType

/**
 * Parameters for configuring the LogMagnitude hardware module.
 *
 * @param inputType    Input DspComplex[T] data type (not used in MagnitudeLog).
 * @param realType     Optional real data type (only relevant for MagnitudeLog implementations).
 * @param outputType   Output data type.
 * @param logType      Optional Look-Up Table (LUT) data type (only used in MagnitudeLog).
 * @param magType      Selection of magnitude approximation type (default: `JPL`).
 * @param lutTableSize Size of the LUT, where the actual table size is :math:`2^{lutTableSize}` (only used for MagnitudeLog).
 * @param addPipeRegs  If `true`, inserts pipeline registers after addition stages.
 * @param mulPipeRegs  If `true`, inserts pipeline registers after multiplication stages.
 * @param trimType     Specifies the trimming strategy applied after arithmetic operations.
 */
case class LogMagnitudeParams[T <: Data](
  inputType   : DspComplex[T],
  realType    : Option[T] = None,
  outputType  : T,
  logType     : Option[T] = None,
  magType     : MagType = JPL,
  lutTableSize: Int = 16,
  addPipeRegs : Boolean = false,
  mulPipeRegs : Boolean = false,
  trimType    : TrimType
)