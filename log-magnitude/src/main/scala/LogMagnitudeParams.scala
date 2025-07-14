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
  inputType   : DspComplex[T],    // Input data type (Complex data)
  realType    : Option[T] = None, // Input real data type. Only relevant for MagnitudeLog
  outputType  : T,                // Output data type
  logType     : Option[T] = None, // Look Up Table data type. Only relevant for MagnitudeLog
  magType     : MagType = JPL,    // Parameter to select magnitude type
  lutTableSize: Int = 16,         // Look Up Table size: LUT size = 2^lutTableSize. Only relevant for MagnitudeLog
  addPipeRegs : Boolean = false,  // Enable Pipeline Registers after addition
  mulPipeRegs : Boolean = false,  // Enable Pipeline Registers after multiplication
  trimType    : TrimType          // TrimType to used after arithmetic operations
)