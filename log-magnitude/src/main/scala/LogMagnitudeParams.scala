package opera.logmagnitude

import chisel3.Data
import dsptools.TrimType

sealed trait MagType

case object MagnitudeJPL extends MagType

// LogMagnitude Parameters
case class LogMagnitudeParams[T <: Data](
  inputType   : T,                      // Input data type (Complex data)
  outputType  : T,                      // Output data type
  logType     : Option[T] = None,
  magType     : MagType = MagnitudeJPL, // Parameter to select used magnitude type
  lutDataWidth: Int = 16,               // Look Up Table data width
  addPipeRegs : Int = 1,                // Number of Pipeline Registers after addition
  mulPipeRegs : Int = 1,                // Number of Pipeline Registers after multiplication
  binaryGrowth: Int = 0,                // Number of bits for binary point growth
  trimType    : TrimType                // TrimType to used after arithmetic operations
)