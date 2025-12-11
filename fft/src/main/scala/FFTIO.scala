package opera.fft

import chisel3._
import chisel3.util._
import dsptools.numbers._

trait HasIO[T <: Data] extends Module {
  val io: FFTIO[T]
}

class FFTIO[T <: Data: Ring](params: FFTParams[T]) extends Bundle {
  val in : DecoupledIO[DspComplex[T]] = Flipped(Decoupled(params.inDataType))
  val out: DecoupledIO[DspComplex[T]] = Decoupled(params.protoIQstages(log2Ceil(params.fftSize) - 1))
  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
  // Control
  val i_size: Option[UInt] = if (params.runTime) Some(Input(UInt(log2Ceil(params.fftSize).W))) else None
  val i_divBy2: Option[Vec[Bool]] = if (params.divBy2Reg) Some(Input(Vec(log2Ceil(params.fftSize), Bool()))) else None
  val i_fft_or_ifft: Option[Bool] = if (params.directionReg) Some(Input(Bool())) else None
  // Status
  val o_overflow: Option[Vec[Bool]] = if (params.overflowReg) Some(Output(Vec(log2Ceil(params.fftSize), Bool()))) else None
}