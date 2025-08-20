package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

case class RadixParams[T <: Data] (
  dataType     : DspComplex[T],
  twiddleType  : DspComplex[T],
  fftSize      : Int,
  decimation   : DecimType,
  overflowReg  : Boolean,
  divBy2Reg    : Boolean,
  divBy2       : Boolean,
  growEnable   : Boolean,
  latency      : Int,
  addPipeRegs  : Int,
  mulPipeRegs  : Int,
  dspMul4      : Boolean,
  delay        : Int,
  bufferAsMem  : Boolean,
  singlePortMem: Boolean,
  trimType     : TrimType,
) {
  require(isPow2(fftSize), f"FFT size must be a power of 2, instead it is: $fftSize")
  require(isPow2(delay)  , f"delay must be a power of 2, instead it is: $delay")
}