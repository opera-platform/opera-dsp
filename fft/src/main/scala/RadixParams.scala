package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint

/**
 * Parameters for one shared SDF stage.
 *
 * @param inDataType    Complex FixedPoint type accepted by the stage input.
 * @param outDataType   Complex FixedPoint type produced by the stage output.
 * @param twiddleType   Complex FixedPoint type used for twiddle coefficients.
 * @param stageSize     Number of samples in this stage schedule. Must be a power of two.
 * @param decimation    SDF decimation mode.
 * @param sdfRadix      SDF radix implementation selector.
 * @param overflowReg   Enables overflow reporting for this stage.
 * @param divBy2Reg     Enables runtime divide-by-two control for this stage.
 * @param divBy2        Static divide-by-two control when `divBy2Reg` is disabled.
 * @param growEnable    If `true`, this stage output type grows by one FixedPoint width bit.
 * @param latency       Complex multiply latency used by stage feedback alignment.
 * @param addPipeRegs   Number of pipeline registers after add/subtract operations.
 * @param mulPipeRegs   Number of pipeline registers after multiplication operations.
 * @param dspMul4       Uses the four-real-multiplier complex multiplier implementation.
 * @param delay         Delay-feedback storage depth. Must be a power of two.
 * @param bufferAsMem   Uses SRAM-backed delay storage instead of registers.
 * @param singlePortMem Uses single-port SRAM for eligible delay storage.
 * @param trimType      Trim mode used for butterfly scaling.
 */
case class RadixParams (
  inDataType   : DspComplex[FixedPoint],
  outDataType  : DspComplex[FixedPoint],
  twiddleType  : DspComplex[FixedPoint],
  stageSize    : Int,
  decimation   : DecimationType,
  sdfRadix     : SDFRadix,
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
