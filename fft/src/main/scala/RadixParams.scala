package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint

case class RadixParams (
  inDataType   : DspComplex[FixedPoint],
  outDataType  : DspComplex[FixedPoint],
  twiddleType  : DspComplex[FixedPoint],
  stageSize    : Int,
  decimation   : DecimationType,
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
  require(isPow2(stageSize), f"Stage size must be a power of 2, instead it is: $stageSize")
  require(isPow2(delay)  , f"delay must be a power of 2, instead it is: $delay")
}
