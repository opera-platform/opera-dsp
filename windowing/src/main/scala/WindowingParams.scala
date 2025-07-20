package opera.windowing

import chisel3._
import dsptools._
import dsptools.numbers._
import fixedpoint._

case class WindowingParams[T <: Data](
  inputType  : DspComplex[T], // Input data type
  outputType : DspComplex[T], // Output data type
  coeffType  : T,             // Coefficient data type
  numPoints  : Int,           // Chirp size
  runTime    : Boolean,       // Use run-time configurable chirp size
  windowFunc : WindowType,    // Window function
  memoryFile : String,        // Text file location in which to store coefficients
  constWindow: Boolean,       // Use ROM to store coefficients
  trimType   : TrimType       // Trim type (after multiplication)
)

object WindowingParams {
  def fixed(
    inputType  : DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType : DspComplex[FixedPoint] = DspComplex(FixedPoint(18.W, 14.BP)),
    coeffType  : FixedPoint = FixedPoint(16.W, 14.BP),
    numPoints  : Int = 1024,
    runTime    : Boolean = true,
    windowFunc : WindowType = NoWindow(),
    memoryFile : String = "",
    constWindow: Boolean = true,
    trimType   : TrimType = Convergent
  ): WindowingParams[FixedPoint] = {
    WindowingParams(
      inputType   = inputType,
      outputType  = outputType,
      coeffType   = coeffType,
      numPoints   = numPoints,
      runTime     = runTime,
      windowFunc  = windowFunc,
      memoryFile  = memoryFile,
      constWindow = constWindow,
      trimType    = trimType
    )
  }
}
