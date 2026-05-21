package opera.fft

import chisel3._
import chisel3.util._
import dsptools.numbers._
import fixedpoint.FixedPoint

class RadixIO(params: RadixParams) extends Bundle {
  val in : DecoupledIO[DspComplex[FixedPoint]] = Flipped(Decoupled(params.inDataType))
  val out: DecoupledIO[DspComplex[FixedPoint]] = Decoupled(params.outDataType)
  // Control
  val o_counter: UInt = Output(UInt(log2Ceil(params.stageSize).W))
  // Optional
  val i_divBy2  : Option[Bool] = if (params.divBy2Reg)   Some(Input(Bool()))  else None
  val o_overflow: Option[Bool] = if (params.overflowReg) Some(Output(Bool())) else None
}
