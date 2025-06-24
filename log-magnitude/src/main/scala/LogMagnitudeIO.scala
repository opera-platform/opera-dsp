package opera.logmagnitude

import chisel3.util.Decoupled
import chisel3.{Bundle, Data, Flipped}
import dsptools.numbers.Real

class LogMagnitudeIO[T <: Data: Real](val params: LogMagnitudeParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.inputType))
  val out = Decoupled(params.outputType)
}

class LogIO[T <: Data: Real](val params: LogMagnitudeParams[T]) extends Bundle {
  val in  = Flipped(Decoupled(params.realType.get))
  val out = Decoupled(params.outputType)
}
