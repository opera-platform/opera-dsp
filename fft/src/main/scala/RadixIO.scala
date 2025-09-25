package opera.fft

import chisel3._
import chisel3.util._
import dsptools.numbers._

class RadixIO[T <: Data: Ring](params: RadixParams[T]) extends Bundle {
  val in : DspComplex[T] = Input(params.dataType)
  val out: DspComplex[T] = Output(params.dataType)
  // Control
  val i_en     : Bool = Input(Bool())
  val o_en     : Bool = Output(Bool())
  val o_counter: UInt = Output(UInt(log2Ceil(params.fftSize).W))
  // Optional
  val i_divBy2  : Option[Bool] = if (params.divBy2Reg)   Some(Input(Bool()))  else None
  val o_overflow: Option[Bool] = if (params.overflowReg) Some(Output(Bool())) else None
}