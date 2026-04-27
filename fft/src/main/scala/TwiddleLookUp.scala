package opera.fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

/** Quarter-wave sine LUT for N = FFT size, i = 0 .. FFT_size/4 */
object QuarterWaveSineLUT {
  def apply[T <: Data : Real](FFT_size: Int, protoTwiddle: DspComplex[T]): Vec[T] = {
    require(FFT_size >= 4,      "FFT size must be at least 4")
    require(FFT_size % 4 == 0,  "FFT size must be divisible by 4")
    val nDiv4 = FFT_size / 4

    VecInit((0 to nDiv4).map { i =>
      DspContext.withTrimType(Convergent) {
        ConvertableTo[T].fromDoubleWithFixedWidth(math.sin(2 * math.Pi * i.toDouble / FFT_size.toDouble), protoTwiddle.real)
      }
    })
  }
}

/** Twiddle factors from Quarter-wave sine LUT */
object TwiddleFromLUT {
  def apply[T <: Data : Real](address: UInt, stageN: Int, FFT_size: Int, LUT: Vec[T]): DspComplex[T] = {
    require(stageN >= 4           ,  "stage must be at least 4")
    require(stageN % 4 == 0       ,  "stage must be divisible by 4")
    require(FFT_size % stageN == 0, s"FFT size ($FFT_size) must be a multiple of stageN ($stageN)")
    require(FFT_size % 4 == 0     ,  "FFT size must be divisible by 4")

    val stride   = FFT_size / stageN
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

    // find quadrant + base angle index
    val nDiv4U = nDiv4.U(idWidth.W)
    val q = if (lowWidth == 0) k(idWidth - 1, 0) else k(idWidth - 1, lowWidth) // quadrant: 0..3
    val m = if (lowWidth == 0) 0.U(idWidth.W) else k(lowWidth - 1, 0).asTypeOf(UInt(idWidth.W))
    // Mirror within quadrant
    val mirror = Mux(q(0), nDiv4U - m, m).asTypeOf(UInt(idWidth.W)) // if odd quadrant, mirror
    val sineID = mirror(log2Ceil(nDiv4 + 1) - 1, 0)
    // Map quarter-wave index to LUT index
    val twiddleID = (sineID * stride.U)(log2Ceil(FFT_size/2)-1,0)
    val sinA = LUT(twiddleID)             // sin(a)
    val cosA = LUT((FFT_size / 4).U - twiddleID) // cos(a) = sin(π/2 - a)
    // find quadrant-based signs
    val cosNeg = q(1) ^ q(0)
    val sinNeg = q(1)
    val cosTheta = Mux(cosNeg, -cosA, cosA)
    val sinTheta = Mux(sinNeg, -sinA, sinA)
    // Return: W_stageN^k = cos(θ) - j sin(θ)
    DspComplex.wire(real = cosTheta, imag = -sinTheta)
  }
}
