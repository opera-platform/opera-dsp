package windowing

import chisel3._
import fixedpoint._
import dsptools._
import dsptools.numbers._

import scala.collection.immutable.Seq


case class WindowingParams[T <: Data](
  dataType:     DspComplex[T], // input data type
  numPoints:   Int, // number of window coefficents
  coeffType:    T, // window coefficients data type
  runTime:     Boolean, // use run time configurable number of points (include fftSize register)
  numMulPipes: Int, // number of pipeline registers after multiplication operator
  windowFunc:  WindowType, // when constWindow is set then this parameter denotes constant window function
  // otherwise it represents window function used to initialize SRAM/Block RAM in run-time configurable mode
  memoryFile:  String, // name of the file where window coefficents are stored
  constWindow: Boolean, // predefined window function stored in ROM is used, no SRAM/Block RAM
  trimType:    TrimType // define trim type after multiplication
)

object WindowingParams {
  def fixed(
    dataWidth:   Int = 16,
    binPoint:    Int = 14,
    numPoints:   Int = 1024,
    runTime:     Boolean = true,
    numMulPipes: Int = 1,
    windowFunc:  WindowType = NoWindow(),
    memoryFile:  String = "",
    constWindow: Boolean = true,
    trimType:    TrimType = Convergent
  ): WindowingParams[FixedPoint] = {
    val dataType = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
    val coeffType = FixedPoint(dataWidth.W, (dataWidth - 2).BP)

    WindowingParams(
      numPoints = numPoints,
      dataType = dataType,
      coeffType = coeffType,
      runTime = runTime,
      numMulPipes = numMulPipes,
      windowFunc = windowFunc,
      memoryFile = memoryFile,
      constWindow = constWindow,
      trimType = trimType
    )
  }
}
