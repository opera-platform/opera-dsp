package opera.logmagnitude

import chisel3.Data
import dsptools.TrimType
import dsptools.numbers.DspComplex

sealed trait MagType

case object JPL extends MagType
case object LogJPL extends MagType
case object Squared extends MagType

// LogMagnitude Parameters
case class LogMagnitudeParams[T <: Data](
  inputType   : DspComplex[T],    // Input data type (Complex data)
  outputType  : T,                // Output data type
  logType     : Option[T] = None,
  magType     : MagType = JPL,    // Parameter to select used magnitude type
  lutDataWidth: Int = 16,         // Look Up Table data width
  addPipeRegs : Boolean = false,  // Number of Pipeline Registers after addition
  mulPipeRegs : Boolean = false,  // Number of Pipeline Registers after multiplication
  binaryGrowth: Int = 0,          // Number of bits for binary point growth
  trimType    : TrimType          // TrimType to used after arithmetic operations
)