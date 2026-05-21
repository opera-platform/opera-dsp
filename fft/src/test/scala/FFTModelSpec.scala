package opera.fft

import chisel3._
import chisel3.util.log2Up
import chiseltest._
import chiseltest.iotesters.PeekPokeTester
import dsptools.{RoundHalfUp, TrimType}
import dsptools.numbers.{Convergent, DspComplex, Floor}
import fixedpoint._
import org.scalatest.flatspec.AnyFlatSpec
import ModelUtils.{FixedFormat, RawComplex}

/**
 * Unit tests for the pure Scala FixedPoint SDF FFT model helpers.
 */
class FFTModelSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of "FFTModel"

  private def annotations   = TestConfig.annotations
  private val radixSeq      = Seq(Radix2, Radix22)
  private val decimationSeq = Seq(DIF, DIT)

  private def fftParams(
      radix       : SDFRadix,
      size        : Int,
      decimation  : DecimationType,
      dspMul4     : Boolean = false,
      dataWidth   : Int = 16,
      binPoint    : Int = 14,
      twiddleWidth: Int = 16,
  ): FFTParams =
    FFTModelTestUtils.fftParams(
      radix        = radix,
      size         = size,
      decimation   = decimation,
      dspMul4      = dspMul4,
      dataWidth    = dataWidth,
      binPoint     = binPoint,
      twiddleWidth = twiddleWidth
    )

  // Checks scalar rounding behavior directly by calling roundShift on positive and negative halfway cases.
  it should "round discarded fixed-point bits with supported trim modes" in {
    assert(ModelUtils.roundShift(BigInt( 3), 1, Floor) == 1)
    assert(ModelUtils.roundShift(BigInt(-3), 1, Floor) == -2)
    assert(ModelUtils.roundShift(BigInt( 3), 1, RoundHalfUp) == 2)
    assert(ModelUtils.roundShift(BigInt(-3), 1, RoundHalfUp) == -1)
    assert(ModelUtils.roundShift(BigInt( 5), 1, Convergent) == 2)
    assert(ModelUtils.roundShift(BigInt( 7), 1, Convergent) == 4)
  }

  // Checks exact twiddle anchor points by generating quarter-cycle LUT entries for a large radix-2 table.
  it should "generate exact radix-2 unit-axis twiddles" in {
    val params   = fftParams(Radix2, 1024, DIF)
    val twFormat = FFTModel.twiddleFormat(params)
    val one      = twFormat.fromDouble(1.0)
    val minusOne = twFormat.fromDouble(-1.0)
    val zero     = BigInt(0)

    assert(FFTModel.radix2Twiddle(  0, 1024, 1024, twFormat) == RawComplex(one, zero))
    assert(FFTModel.radix2Twiddle(256, 1024, 1024, twFormat) == RawComplex(zero, minusOne))
    assert(FFTModel.radix2Twiddle(512, 1024, 1024, twFormat) == RawComplex(minusOne, zero))
  }

  // Checks hardware multiplier semantics by probing Utils.complexMul and comparing raw bits to the Scala model.
  it should "match hardware complex multiplier for representative raw samples" in {
    val params     = fftParams(Radix2, 16, DIF)
    val dataFormat = FFTModel.inputFormat(params)
    val twFormat   = FFTModel.twiddleFormat(params)
    val samples    = Seq(
      RawComplex( 37, -19) -> FFTModel.radix2Twiddle(1,  8, 16, twFormat),
      RawComplex(-53,  41) -> FFTModel.radix2Twiddle(3, 16, 16, twFormat),
      RawComplex( 12,   7) -> RawComplex(twFormat.fromDouble(1.0), 0),
    )

    test(new ComplexMulProbe(params, Convergent))
      .withAnnotations(annotations)
      .runPeekPoke(new ComplexMulProbeTester(_, dataFormat, twFormat, Convergent, params.dspMul4, samples))
  }

  // Checks that the full FFT model reports per-stage overflow vectors while preserving sample output behavior.
  it should "report FFT model overflow by cycle and stage" in {
    val configs = for {
      radix      <- radixSeq
      decimation <- decimationSeq
    } yield FFTModelTestUtils.ModelComparisonConfiguration(radix, decimation, size = if (radix == Radix22) 16 else 8)

    configs.foreach { configuration =>
      val params = fftParams(
        configuration.radix,
        configuration.size,
        configuration.decimation,
        dataWidth    = 8,
        binPoint     = 4,
        twiddleWidth = 8
      ).copy(overflowReg = true)
      val stageCount = log2Up(params.fftSize)
      val format     = FFTModel.inputFormat(params)
      val zero       = RawComplex(0, 0)
      val hot        = RawComplex(format.maxRaw, format.maxRaw)

      val quiet = FFTModel(params, Vector.fill(params.fftSize * 2)(zero))
      assert(!quiet.anyOverflow, s"unexpected overflow for quiet ${configuration.radix} ${configuration.decimation}")
      assert(quiet.overflowByCycle.forall(_.length == stageCount))

      val stressedInput = Vector.fill(params.fftSize * 3)(hot)
      val stressed = FFTModel(params, stressedInput)
      assert(stressed.samples.length == stressedInput.length)
      assert(stressed.overflowByCycle.nonEmpty)
      assert(stressed.overflowByCycle.forall(_.length == stageCount))
      assert(stressed.anyOverflow, s"expected overflow for ${configuration.radix} ${configuration.decimation}")
      assert(stressed.stageOverflow(0).contains(true), s"expected stage 0 overflow for ${configuration.radix} ${configuration.decimation}")
    }
  }
}

/**
 * Small hardware probe that exposes `Utils.complexMul` for model comparison.
 *
 * @param params   FFT parameters that define data and twiddle formats.
 * @param trimType Rounding or truncation mode used by the multiplier.
 */
private final class ComplexMulProbe(params: FFTParams, trimType: TrimType) extends Module {
  private val dataFormat  = FFTModel.inputFormat(params)
  private val twFormat    = FFTModel.twiddleFormat(params)
  private val dataType    = FixedPoint(dataFormat.width.W, dataFormat.binaryPoint.BP)
  private val twiddleType = FixedPoint(twFormat.width.W, twFormat.binaryPoint.BP)
  val io = IO(new Bundle {
    val inReal  = Input(SInt(dataFormat.width.W))
    val inImag  = Input(SInt(dataFormat.width.W))
    val twReal  = Input(SInt(twFormat.width.W))
    val twImag  = Input(SInt(twFormat.width.W))
    val outReal = Output(SInt(dataFormat.width.W))
    val outImag = Output(SInt(dataFormat.width.W))
  })

  val in = DspComplex(
    io.inReal.asTypeOf(dataType),
    io.inImag.asTypeOf(dataType)
  )
  val tw = DspComplex(
    io.twReal.asTypeOf(twiddleType),
    io.twImag.asTypeOf(twiddleType)
  )
  val out = Utils.complexMul(
    in,
    tw,
    params.inDataType,
    params.numAddPipes,
    params.numMulPipes,
    trimType,
    params.dspMul4
  )
  io.outReal := out.real.asSInt
  io.outImag := out.imag.asSInt
}

/**
 * Drives representative raw values through `ComplexMulProbe`.
 *
 * @param dut      Complex multiplier probe.
 * @param format   FixedPoint format of input and output samples.
 * @param twFormat FixedPoint format of twiddle coefficients.
 * @param trimType Rounding or truncation mode used by the multiplier.
 * @param dspMul4  If `true`, checks the four-real-multiply implementation.
 * @param samples  Input and twiddle sample pairs to verify.
 */
private final class ComplexMulProbeTester(
    dut     : ComplexMulProbe,
    format  : FixedFormat,
    twFormat: FixedFormat,
    trimType: TrimType,
    dspMul4 : Boolean,
    samples : Seq[(RawComplex, RawComplex)],
) extends PeekPokeTester(dut) {
  reset(2)
  samples.foreach { case (input, twiddle) =>
    poke(dut.io.inReal, input.real)
    poke(dut.io.inImag, input.imag)
    poke(dut.io.twReal, twiddle.real)
    poke(dut.io.twImag, twiddle.imag)
    step(6)
    val expected = ModelUtils.complexMul(input, twiddle, format, twFormat, trimType, dspMul4)
    val actual = RawComplex(format.wrap(peek(dut.io.outReal)), format.wrap(peek(dut.io.outImag)))
    assert(actual == expected, s"complexMul mismatch input=$input twiddle=$twiddle expected=$expected actual=$actual")
  }
}
