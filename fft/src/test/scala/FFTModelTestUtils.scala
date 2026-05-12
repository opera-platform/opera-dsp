package opera.fft

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.signal.fourierTr
import chisel3._
import chisel3.util.log2Up
import dsptools.{RoundHalfUp, TrimType}
import dsptools.numbers.{Convergent, DspComplex, Floor}
import fixedpoint._

import java.io.File
import ModelUtils.{FixedFormat, RawComplex}

object FFTModelTestUtils {
  final case class ModelComparisonConfiguration(radix: SDFRadix, decimation: DecimationType, size: Int)
  final case class HighPrecisionConfiguration(radix: SDFRadix, decimation: DecimationType, size: Int, pattern: InputPatterns.FftFramePattern)
  final case class RawBitFixture(input: Vector[RawComplex], expected: Vector[RawComplex], compareStart: Int)

  def fftParams(
      radix       : SDFRadix,
      size        : Int,
      decimation  : DecimationType,
      use4Muls    : Boolean = false,
      mixedTrim   : Boolean = false,
      dataWidth   : Int = 16,
      binPoint    : Int = 14,
      twiddleWidth: Int = 16,
      growEnable  : Seq[Boolean] = Seq.empty,
  ): FFTParams = {
    val stages = log2Up(size)
    val stageTrims =
      if (mixedTrim) Array.tabulate[TrimType](stages)(i => if (i % 3 == 0) Convergent else if (i % 3 == 1) Floor else RoundHalfUp)
      else Array.fill[TrimType](stages)(Convergent)
    val twiddleTrims =
      if (mixedTrim) Array.tabulate[TrimType](stages)(i => if (i % 3 == 0) Floor else if (i % 3 == 1) Convergent else RoundHalfUp)
      else Array.fill[TrimType](stages)(Convergent)

    FFTParams(
      inDataType       = DspComplex(FixedPoint(dataWidth.W, binPoint.BP)),
      twiddleType      = DspComplex(FixedPoint(twiddleWidth.W, (twiddleWidth - 2).BP)),
      fftSize          = size,
      numAddPipes      = 1,
      numMulPipes      = 1,
      decimation       = decimation,
      trimType         = Convergent,
      stageTrimTypes   = stageTrims.toSeq,
      twiddleTrimTypes = twiddleTrims.toSeq,
      growEnable       = growEnable,
      sdfRadix         = radix,
      use4Muls         = use4Muls
    )
  }

  def deterministicInput(params: FFTParams, frames: Int, seed: Long, amplitudeRaw: Int = 64): Vector[RawComplex] =
    InputPatterns.deterministicFftFrames(params, seed, frames, amplitudeRaw = amplitudeRaw)

  def highPrecisionFrame(
      params : FFTParams,
      pattern: InputPatterns.FftFramePattern,
  ): Vector[RawComplex] =
    InputPatterns.fftFrame(params, pattern)

  def repeatedDutInput(
      params: FFTParams,
      frame : Vector[RawComplex],
      frames: Int,
  ): Vector[RawComplex] = {
    val dutFrame = if (params.decimation == DIT) BitReverseUtils.bitReverse(frame) else frame
    Vector.fill(frames)(dutFrame).flatten
  }

  def rawBitFixture(
      params      : FFTParams,
      seed        : Long,
      amplitudeRaw: Int = 64,
      frames      : Int = 3,
      compareStart: Int = 0,
  ): RawBitFixture = {
    val frame = deterministicInput(params, frames = 1, seed = seed, amplitudeRaw = amplitudeRaw).take(params.fftSize)
    val input = repeatedDutInput(params, frame, frames)
    RawBitFixture(input, FFTModel(params, input).checkedFrame(params.fftSize), compareStart)
  }

  def wrapperRawBitFixture(params: FFTParams, seed: Long): RawBitFixture =
    rawBitFixture(
      params       = params,
      seed         = seed,
      amplitudeRaw = 32,
    )

  def bitReverseTopRawBitFixture(
      params      : FFTParams,
      seed        : Long,
      amplitudeRaw: Int = 64,
      frames      : Int = 5,
  ): RawBitFixture = {
    require(params.useBitReverse, "bit-reversal top fixture requires useBitReverse = true")

    val frame        = deterministicInput(params, frames = 1, seed = seed, amplitudeRaw = amplitudeRaw).take(params.fftSize)
    val topInput     = Vector.fill(frames)(frame).flatten
    val coreParams   = params.copy(useBitReverse = false)
    val coreInput    = if (params.decimation == DIT) BitReverseUtils.bitReverseFrameGroups(topInput, params.fftSize) else topInput
    val coreOutput   = FFTModel(coreParams, coreInput).checkedFrame(params.fftSize)
    val topOutput    = if (params.decimation == DIF) BitReverseUtils.bitReverseFrameGroups(coreOutput, params.fftSize) else coreOutput
    val compareStart = 0

    RawBitFixture(topInput, topOutput.drop(compareStart).take(params.fftSize), compareStart)
  }

  def overflowRawBitFixture(params: FFTParams, frames: Int = 4): RawBitFixture = {
    val format       = FFTModel.inputFormat(params)
    val frame        = Vector.fill(params.fftSize)(RawComplex(format.maxRaw, format.maxRaw))
    val input        = repeatedDutInput(params, frame, frames)
    val compareStart = 0
    val model        = FFTModel(params, input)

    require(model.anyOverflow, s"overflow fixture did not overflow for ${params.sdfRadix.label} ${params.decimation}")
    RawBitFixture(input, model.checkedFrame(params.fftSize), compareStart)
  }

  def overflowParams(
      radix       : SDFRadix = Radix2,
      size        : Int = 4,
      decimation  : DecimationType = DIF,
      dataWidth   : Int = 8,
      binPoint    : Int = 6,
      twiddleWidth: Int = 8,
  ): FFTParams =
    fftParams(
      radix        = radix,
      size         = size,
      decimation   = decimation,
      dataWidth    = dataWidth,
      binPoint     = binPoint,
      twiddleWidth = twiddleWidth
    ).copy(
      divBy2 = Seq.fill(log2Up(size))(false),
      overflowReg = true
    )

  def floatingPointFrame(
      params: FFTParams,
      frame : Vector[RawComplex],
  ): Vector[Complex] = {
    val inFormat = FFTModel.inputFormat(params)
    val natural  = frame.map(ModelUtils.rawToComplex(inFormat, _))
    val fft      = fourierTr(DenseVector(natural.toArray)).toScalaVector.toVector
    val ordered  = if (params.decimation == DIF) BitReverseUtils.bitReverse(fft) else fft
    val scale    = math.pow(2.0, params.stageGrowEnable.indices.count(i => !params.stageGrowEnable(i) && params.stageDivBy2(i)))
    ordered.map(sample => Complex(sample.real / scale, sample.imag / scale))
  }

  def compareModelToFloatingPoint(
      params  : FFTParams,
      frame   : Vector[RawComplex],
      tol     : Double,
      plotName: String,
  ): Option[File] = {
    val modelInput = repeatedDutInput(params, frame, frames = 3)
    val modelFrame = FFTModel(params, modelInput).checkedFrame(params.fftSize)
    val expected   = floatingPointFrame(params, frame)
    val outFormat  = FFTModel.stageFormat(params, log2Up(params.fftSize) - 1)
    val modelFft   = modelFrame.map(ModelUtils.rawToComplex(outFormat, _))

    assert(modelFrame.length == params.fftSize)
    modelFft.zip(expected).zipWithIndex.foreach { case ((actual, expectedSample), index) =>
      assert(
        math.abs(actual.real - expectedSample.real) <= tol,
        s"real mismatch at sample $index: actual=${actual.real}, expected=${expectedSample.real}, tol=$tol"
      )
      assert(
        math.abs(actual.imag - expectedSample.imag) <= tol,
        s"imag mismatch at sample $index: actual=${actual.imag}, expected=${expectedSample.imag}, tol=$tol"
      )
    }

    writeFloatingPointPlotIfEnabled(plotName, modelFft, expected)
  }

  private def writeFloatingPointPlotIfEnabled(name: String, model: Vector[Complex], floatingPoint: Vector[Complex]): Option[File] =
    if (TestConfig.plot) {
      val safeName = name.replace("^", "x").replaceAll("[^A-Za-z0-9_.-]", "-")
      Some(PlotUtils.writePlot(
        output = new File(TestConfig.plotDirectory, s"$safeName.png"),
        title  = name,
        model  = model,
        breeze = floatingPoint
      ))
    } else {
      None
    }

}
