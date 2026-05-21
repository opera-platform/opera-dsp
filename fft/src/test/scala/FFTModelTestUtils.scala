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
import ModelUtils.RawComplex

object FFTModelTestUtils {
  final case class ModelComparisonConfiguration(radix: SDFRadix, decimation: DecimationType, size: Int)
  final case class HighPrecisionConfiguration(radix: SDFRadix, decimation: DecimationType, size: Int, pattern: InputPatterns.FftFramePattern)

  def fftParams(
      radix       : SDFRadix,
      size            : Int,
      decimation    : DecimationType,
      dspMul4       : Boolean = false,
      mixedTrim     : Boolean = false,
      dataWidth     : Int = 16,
      binPoint      : Int = 14,
      twiddleWidth  : Int = 16,
      growEnable    : Seq[Boolean] = Seq.empty,
      numAddPipes   : Int = 1,
      numMulPipes   : Int = 1,
      minSRAMdepth  : Int = 0,
      singlePortSRAM: Boolean = false,
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
      numAddPipes      = numAddPipes,
      numMulPipes      = numMulPipes,
      decimation       = decimation,
      trimType         = Convergent,
      stageTrimTypes   = stageTrims.toSeq,
      twiddleTrimTypes = twiddleTrims.toSeq,
      growEnable       = growEnable,
      sdfRadix         = radix,
      dspMul4          = dspMul4,
      minSRAMdepth     = minSRAMdepth,
      singlePortSRAM   = singlePortSRAM
    )
  }

  def deterministicInput(params: FFTParams, frames: Int, seed: Long, amplitudeRaw: Int = 64): Vector[RawComplex] =
    InputPatterns.deterministicFftFrames(params, seed, frames, amplitudeRaw = amplitudeRaw)

  def shiftedFramePattern(pattern: InputPatterns.FftFramePattern, frameIndex: Int): InputPatterns.FftFramePattern =
    pattern.copy(
      tones = pattern.tones.map(tone => tone.copy(phaseRadians = tone.phaseRadians + frameIndex.toDouble * 0.37)),
      noise = pattern.noise.map(noise => noise.copy(seed = noise.seed + frameIndex.toLong))
    )

  def dutInputFrame(params: FFTParams, frame: Vector[RawComplex]): Vector[RawComplex] =
    if (params.decimation == DIT) BitReverseUtils.bitReverse(frame) else frame

  def dutInputFrames(params: FFTParams, frames: Seq[Vector[RawComplex]]): Vector[RawComplex] =
    frames.flatMap(frame => dutInputFrame(params, frame)).toVector

  def patternedDutInput(
      params : FFTParams,
      pattern: InputPatterns.FftFramePattern,
      frames : Int,
  ): Vector[RawComplex] =
    dutInputFrames(
      params,
      Vector.tabulate(frames)(frameIndex => InputPatterns.fftFrame(params, shiftedFramePattern(pattern, frameIndex)))
    )

  def repeatedDutInput(
      params: FFTParams,
      frame : Vector[RawComplex],
      frames: Int,
  ): Vector[RawComplex] = {
    val dutFrame = dutInputFrame(params, frame)
    Vector.fill(frames)(dutFrame).flatten
  }

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

    PlotUtils.writePlotIfEnabled(plotName, modelFft, expected)
  }
}
