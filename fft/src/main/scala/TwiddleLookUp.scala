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
  private def radix22TwiddleIDs(stageN: Int): Seq[Int] = {
    require(stageN >= 4, "FFT stage size must be at least 4")
    val q = stageN / 4
    Seq.fill(q)(0) ++ Seq.tabulate(q)(i => i * 2) ++ Seq.tabulate(q)(i => i) ++ Seq.tabulate(q)(i => i * 3)
  }

  def apply[T <: Data : Real](address: UInt, stageN: Int, FFT_size: Int, LUT: Vec[T]): DspComplex[T] = {
    require(stageN >= 4           ,  "stage must be at least 4")
    require(stageN % 4 == 0       ,  "stage must be divisible by 4")
    require(FFT_size % stageN == 0, s"FFT size ($FFT_size) must be a multiple of stageN ($stageN)")
    require(FFT_size % 4 == 0     ,  "FFT size must be divisible by 4")

    val stride   = FFT_size / stageN
    val nDiv4    = stageN / 4
    val idWidth = log2Ceil(stageN)
    // logical twiddle index k for this stage (radix-2^2 pattern)
    val twiddleIDs: Seq[Int] = radix22TwiddleIDs(stageN)
    val twiddleIdxVec = VecInit(twiddleIDs.map(i => i.U(idWidth.W)))
    val k = twiddleIdxVec(address)
    // find quadrant + base angle index
    val nDiv4U = nDiv4.U(idWidth.W)
    val q = (k / nDiv4U)(1, 0) // quadrant: 0..3
    val m = k % nDiv4U         // 0 .. nDiv4-1
    // Mirror within quadrant
    val mirror = Mux(q(0), nDiv4U - m, m).asTypeOf(UInt(idWidth.W)) // if odd quadrant, mirror
    val sineID = mirror(log2Ceil(nDiv4 + 1) - 1, 0)
    // Map quarter-wave index to LUT index
    val twiddleID = (sineID * stride.U)(log2Ceil(FFT_size/2)-1,0)
    val sinA = LUT(twiddleID)             // sin(a)
    val cosA = LUT((FFT_size / 4).U - twiddleID) // cos(a) = sin(π/2 - a)
    // find quadrant-based signs
    val cosNeg = Wire(Bool())
    val sinNeg = Wire(Bool())
    cosNeg := false.B
    sinNeg := false.B
    switch(q) {
      is(0.U) { cosNeg := false.B; sinNeg := false.B } // + +
      is(1.U) { cosNeg := true.B;  sinNeg := false.B } // - +
      is(2.U) { cosNeg := true.B;  sinNeg := true.B  } // - -
      is(3.U) { cosNeg := false.B; sinNeg := true.B  } // + -
    }
    val cosTheta = Mux(cosNeg, -cosA, cosA)
    val sinTheta = Mux(sinNeg, -sinA, sinA)
    // Return: W_stageN^k = cos(θ) - j sin(θ)
    DspComplex.wire(real = cosTheta, imag = -sinTheta)
  }
}

