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

// LogMagnitude Parameters
case class LogMagnitudeParams[T <: Data](
  inputType   : DspComplex[T],    // Input data type (Complex data)
  realType    : Option[T] = None, // Input data type (MagnitudeLog input)
  outputType  : T,                // Output data type
  logType     : Option[T] = None, // Look Up Table data type
  magType     : MagType = JPL,    // Parameter to select used magnitude type
  lutTableSize: Int = 16,         // Look Up Table size: 2^lutTableSize
  addPipeRegs : Boolean = false,  // Enable Pipeline Registers after addition
  mulPipeRegs : Boolean = false,  // Enable Pipeline Registers after multiplication
  binaryGrowth: Int = 0,          // Number of bits for binary point growth, used for Magnitude Squared
  trimType    : TrimType          // TrimType to used after arithmetic operations
)