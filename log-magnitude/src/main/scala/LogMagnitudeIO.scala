package opera.logmagnitude

import chisel3.util.Decoupled
import chisel3.{Bundle, Data, Flipped}
import dsptools.numbers.{DspComplex, Real}

class LogMagnitudeIO[T <: Data: Real](val params: LogMagnitudeParams[T]) extends Bundle {
  val in = Flipped(Decoupled(DspComplex(params.inputType)))
  val out = Decoupled(params.outputType)
}