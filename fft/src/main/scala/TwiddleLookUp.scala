package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint

/** Quarter-wave sine LUT for N = FFT size, i = 0 .. FFT_size/4 */
object QuarterWaveSineLUT {
  def apply(FFT_size: Int, protoTwiddle: DspComplex[FixedPoint]): Vec[FixedPoint] = {
    require(FFT_size >= 4,      "FFT size must be at least 4")
    require(FFT_size % 4 == 0,  "FFT size must be divisible by 4")
    val nDiv4 = FFT_size / 4

    VecInit((0 to nDiv4).map { i =>
      DspContext.withTrimType(Convergent) {
        ConvertableTo[FixedPoint].fromDoubleWithFixedWidth(math.sin(2 * math.Pi * i.toDouble / FFT_size.toDouble), protoTwiddle.real)
      }
    })
  }
}

private[fft] object QuarterWaveTwiddle {
  def requireValidConfig(stageN: Int, FFT_size: Int): Unit = {
    require(stageN >= 4           ,  "stage must be at least 4")
    require(stageN % 4 == 0       ,  "stage must be divisible by 4")
    require(FFT_size % stageN == 0, s"FFT size ($FFT_size) must be a multiple of stageN ($stageN)")
    require(FFT_size % 4 == 0     ,  "FFT size must be divisible by 4")
  }

  def fromLogicalIndex(k: UInt, stageN: Int, FFT_size: Int, LUT: Vec[FixedPoint]): DspComplex[FixedPoint] = {
    requireValidConfig(stageN, FFT_size)

    val stride   = FFT_size / stageN
    val nDiv4    = stageN / 4
    val idWidth  = log2Ceil(stageN)
    val lowWidth = log2Ceil(nDiv4)

    val logicalIndex = k(idWidth - 1, 0)
    val nDiv4U = nDiv4.U(idWidth.W)
    val q = if (lowWidth == 0) logicalIndex(idWidth - 1, 0) else logicalIndex(idWidth - 1, lowWidth)
    val m = if (lowWidth == 0) 0.U(idWidth.W) else logicalIndex(lowWidth - 1, 0).asTypeOf(UInt(idWidth.W))
    val mirror = Mux(q(0), nDiv4U - m, m).asTypeOf(UInt(idWidth.W))
    val sineID = mirror(log2Ceil(nDiv4 + 1) - 1, 0)

    val twiddleIDWidth = log2Ceil(FFT_size / 2)
    val scaledSineID = if (isPowerOfTwo(stride)) sineID << log2Ceil(stride) else sineID * stride.U
    val twiddleID = scaledSineID(twiddleIDWidth - 1, 0)

    val sinA = LUT(twiddleID)
    val cosA = LUT((FFT_size / 4).U - twiddleID)
    val cosNeg = q(1) ^ q(0)
    val sinNeg = q(1)
    val cosTheta = Mux(cosNeg, -cosA, cosA)
    val sinTheta = Mux(sinNeg, -sinA, sinA)

    DspComplex.wire(real = cosTheta, imag = -sinTheta)
  }

  private def isPowerOfTwo(value: Int): Boolean =
    value > 0 && (value & (value - 1)) == 0
}

/** Radix-2^2 twiddle factors from the shared quarter-wave sine LUT. */
object Radix22TwiddleFromLUT {
  def apply(address: UInt, stageN: Int, FFT_size: Int, LUT: Vec[FixedPoint]): DspComplex[FixedPoint] = {
    QuarterWaveTwiddle.requireValidConfig(stageN, FFT_size)

    val nDiv4    = stageN / 4
    val idWidth  = log2Ceil(stageN)
    val lowWidth = log2Ceil(nDiv4)

    // logical twiddle index k for this radix-2^2 stage:
    // address quadrants select 0, 2a, a, or 3a.
    val addressInStage = address(idWidth - 1, 0)
    val addressQuadrant = addressInStage(idWidth - 1, idWidth - 2)
    val a = if (lowWidth == 0) 0.U(idWidth.W) else addressInStage(lowWidth - 1, 0).asTypeOf(UInt(idWidth.W))
    val k = MuxLookup(addressQuadrant, 0.U(idWidth.W))(Seq(
      0.U -> 0.U(idWidth.W),
      1.U -> (a << 1).asTypeOf(UInt(idWidth.W)),
      2.U -> a,
      3.U -> (a + (a << 1)).asTypeOf(UInt(idWidth.W))
    ))

    QuarterWaveTwiddle.fromLogicalIndex(k, stageN, FFT_size, LUT)
  }
}

/** Direct radix-2 twiddle factor from the shared quarter-wave sine LUT. */
object Radix2TwiddleFromLUT {
  def apply(address: UInt, stageN: Int, FFT_size: Int, LUT: Vec[FixedPoint]): DspComplex[FixedPoint] = {
    QuarterWaveTwiddle.fromLogicalIndex(address, stageN, FFT_size, LUT)
  }
}
