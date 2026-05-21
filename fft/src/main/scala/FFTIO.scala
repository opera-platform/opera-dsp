package opera.fft

import chisel3._
import chisel3.util._
import dsptools.numbers._
import fixedpoint.FixedPoint

trait HasIO extends Module {
  val io: FFTIO
}

/**
 * FFT stream, runtime control, and status interface.
 *
 * The top-level [[FFT]] wrapper and the internal radix cores use the same bundle.
 * Optional runtime configuration and overflow status ports are generated only when
 * enabled by [[FFTParams]].
 *
 * @param params FFT configuration used to size the stream, control, and status signals.
 */
class FFTIO(params: FFTParams) extends Bundle {
  private val hasRuntimeConfig = params.runTime || params.divBy2Reg || params.directionReg
  // Input/output stream
  val in : DecoupledIO[DspComplex[FixedPoint]] = Flipped(Decoupled(params.inDataType))
  val out: DecoupledIO[DspComplex[FixedPoint]] = Decoupled(params.fftOutputType)
  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
  // Control
  val i_load_cfg: Option[Bool] = if (hasRuntimeConfig) Some(Input(Bool())) else None
  val i_size: Option[UInt] = if (params.runTime) Some(Input(UInt(log2Ceil(params.fftSize).W))) else None
  val i_divBy2: Option[Vec[Bool]] = if (params.divBy2Reg) Some(Input(Vec(log2Ceil(params.fftSize), Bool()))) else None
  val i_fft_or_ifft: Option[Bool] = if (params.directionReg) Some(Input(Bool())) else None
  // Status
  val o_overflow: Option[Vec[Bool]] = if (params.overflowReg) Some(Output(Vec(log2Ceil(params.fftSize), Bool()))) else None
}
