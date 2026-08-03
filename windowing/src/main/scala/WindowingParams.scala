package opera.windowing

import chisel3._
import dsptools._
import dsptools.numbers._
import fixedpoint._

sealed trait RomStyle {
  def jsonName: String
}

/** Compatibility name for the asynchronous Chisel ROM implementation. */
case object Distributed extends RomStyle {
  override val jsonName: String = "Distributed"
}

case object Synchronous extends RomStyle {
  override val jsonName: String = "Synchronous"
}

case class WindowingParams[T <: Data](
    inputType: DspComplex[T], // Input data type
    outputType: DspComplex[T], // Output data type
    coeffType: T, // Coefficient data type
    numPoints: Int, // Chirp size
    runTime: Boolean, // Use run-time configurable chirp size
    windowFunc: WindowType, // Window function
    memoryFile: String, // Text file location in which to store coefficients
    constWindow: Boolean, // Use ROM to store coefficients
    trimType: TrimType, // Trim type (after multiplication)
    mulPipeRegs: Int = 0, // Register the full-precision products
    roundPipeRegs: Int = 0, // Register the rounded outputs
    romStyle: RomStyle = Distributed,
    foldSymmetric: Boolean = false
) {
  require(numPoints > 0, "Windowing numPoints must be positive")
  require(mulPipeRegs == 0 || mulPipeRegs == 1, "Windowing mulPipeRegs must be 0 or 1")
  require(roundPipeRegs == 0 || roundPipeRegs == 1,
    "Windowing roundPipeRegs must be 0 or 1")
  require(roundPipeRegs <= mulPipeRegs,
    "Windowing roundPipeRegs must be less than or equal to mulPipeRegs")
  require(!(windowFunc == NoWindow() && !constWindow),
    "Windowing NoWindow requires constWindow=true; writable coefficient RAM is invalid")
  require(!(romStyle == Synchronous && mulPipeRegs == 0),
    "Windowing synchronous ROM requires mulPipeRegs=1")
  require(!foldSymmetric ||
    (constWindow && !runTime && romStyle == Synchronous && windowFunc.function.nonEmpty),
    "Windowing foldSymmetric requires a fixed-size constant synchronous ROM window")
  require(!foldSymmetric || windowFunc.periodicity.nonEmpty,
    "Windowing foldSymmetric requires a built-in window with known periodicity")

  (inputType.real, outputType.real, coeffType) match {
    case (in: FixedPoint, out: FixedPoint, coeff: FixedPoint) =>
      val inputBP = in.binaryPoint.get
      val outputBP = out.binaryPoint.get
      val coeffBP = coeff.binaryPoint.get
      require(outputBP >= inputBP,
        s"Windowing output binary point ($outputBP) must cover input binary point " +
          s"($inputBP) exactly")
      require(out.getWidth - outputBP >= in.getWidth - inputBP,
        s"Windowing output integer range (${out.getWidth - outputBP} bits) must cover " +
          s"input integer range (${in.getWidth - inputBP} bits)")
      require(outputBP <= inputBP + coeffBP,
        s"Windowing output binary point ($outputBP) cannot exceed product binary point " +
          s"(${inputBP + coeffBP})")
    case _ => ()
  }

  if (foldSymmetric) {
    val coeff = coeffType.asInstanceOf[FixedPoint]
    val coefficients = windowFunc.function.getOrElse(
      throw new IllegalArgumentException("Windowing folded window is missing coefficients"))
    val periodic = windowFunc.periodicity.getOrElse(
      throw new IllegalArgumentException("Windowing folded window is missing periodicity"))
    require(
      WindowCoefficientQuantizer.isSymmetric(
        coefficients,
        coeff.getWidth,
        coeff.binaryPoint.get,
        periodic
      ),
      "Windowing foldSymmetric requires bit-exact symmetry after coefficient quantization"
    )
  }
}

object WindowingParams {
  def fixed(
      inputType: DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
      outputType: DspComplex[FixedPoint] = DspComplex(FixedPoint(18.W, 14.BP)),
      coeffType: FixedPoint = FixedPoint(16.W, 14.BP),
      numPoints: Int = 1024,
      runTime: Boolean = true,
      windowFunc: WindowType = NoWindow(),
      memoryFile: String = "",
      constWindow: Boolean = true,
      trimType: TrimType = Convergent,
      mulPipeRegs: Int = 0,
      roundPipeRegs: Int = 0,
      romStyle: RomStyle = Distributed,
      foldSymmetric: Boolean = false
  ): WindowingParams[FixedPoint] = {
    WindowingParams(
      inputType = inputType,
      outputType = outputType,
      coeffType = coeffType,
      numPoints = numPoints,
      runTime = runTime,
      windowFunc = windowFunc,
      memoryFile = memoryFile,
      constWindow = constWindow,
      trimType = trimType,
      mulPipeRegs = mulPipeRegs,
      roundPipeRegs = roundPipeRegs,
      romStyle = romStyle,
      foldSymmetric = foldSymmetric
    )
  }
}
