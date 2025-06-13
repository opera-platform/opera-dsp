package opera.windowing

import chisel3._
import dsptools._
import dsptools.numbers._
import fixedpoint._

case class WindowingParams[T <: Data](
  dataType   : DspComplex[T], // Input data type
  numPoints  : Int,           // Chirp size
  coeffType  : T,             // Coefficient data type
  runTime    : Boolean,       // Use run-time configurable chirp size
  windowFunc : WindowType,    // Window function
  memoryFile : String,        // Text file location in which to store coefficients
  constWindow: Boolean,       // Use ROM to store coefficients
  trimType   : TrimType       // Trim type (after multiplication)
)

object WindowingParams {
  def fixed(
    dataWidth  : Int = 16,
    binPoint   : Int = 14,
    numPoints  : Int = 1024,
    runTime    : Boolean = true,
    windowFunc : WindowType = NoWindow(),
    memoryFile : String = "",
    constWindow: Boolean = true,
    trimType   : TrimType = Convergent
  ): WindowingParams[FixedPoint] = {
    val dataType  = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
    val coeffType = FixedPoint(dataWidth.W, (dataWidth - 2).BP)

    WindowingParams(
      numPoints   = numPoints,
      dataType    = dataType,
      coeffType   = coeffType,
      runTime     = runTime,
      windowFunc  = windowFunc,
      memoryFile  = memoryFile,
      constWindow = constWindow,
      trimType    = trimType
    )
  }
}
