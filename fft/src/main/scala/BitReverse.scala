package opera.fft

import chisel3._
import chisel3.util._
import dsptools.numbers.{DspComplex, Real}

case class BitReverseParams[T <: Data](

  requireIsChiselType(dataType)
}


class BitReverseIO[T <: Data: Real](val params: BitReverseParams[T]) extends Bundle {
  val in: DecoupledIO[DspComplex[T]] = Flipped(Decoupled(params.dataType))
  val out: DecoupledIO[DspComplex[T]] = Decoupled(params.dataType)
  val i_samples: Option[UInt] = if (params.adjustableSize) Some(Input(UInt(log2Up(params.pingPongSize + 1).W))) else None

  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
  
}

class BitReverse[T <: Data: Real](val params: BitReverseParams[T]) extends Module {
  val io = IO(new BitReverseIO(params))

}

