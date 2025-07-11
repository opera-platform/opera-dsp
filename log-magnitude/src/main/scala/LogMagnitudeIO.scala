package opera.logmagnitude

import chisel3.util.{Decoupled, DecoupledIO}
import chisel3.{Bool, Bundle, Data, Flipped, Input, Output}
import dsptools.numbers.{DspComplex, Real}

class LogMagnitudeIO[T <: Data: Real](val params: LogMagnitudeParams[T]) extends Bundle {
  val in : DecoupledIO[DspComplex[T]] = Flipped(Decoupled(params.inputType))
  val out: DecoupledIO[T] = Decoupled(params.outputType)
  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
  val sel: Option[Bool] = if (params.magType == LogSquaredJPL || params.magType == LogJPLSquared) Some(Input(Bool())) else None
}

class LogIO[T <: Data: Real](val params: LogMagnitudeParams[T]) extends Bundle {
  val in : DecoupledIO[T] = Flipped(Decoupled(params.realType.get))
  val out: DecoupledIO[T] = Decoupled(params.outputType)
  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
}
