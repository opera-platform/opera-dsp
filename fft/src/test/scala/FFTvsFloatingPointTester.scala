package opera.fft

import breeze.math.Complex
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions

import scala.collection.mutable.ArrayBuffer
import ModelUtils.RawComplex

private[fft] sealed trait FloatingPointCheck {
  def label: String
}

private[fft] case object FirstValidOutputFrame extends FloatingPointCheck {
  override val label: String = "first valid output frame"
}

private[fft] case object InitialStoring extends FloatingPointCheck {
  override val label: String = "R22 initial store output frame"
}

private[fft] case object MultipleAcceptedFrames extends FloatingPointCheck {
  override val label: String = "multiple accepted frames produce output frames"
}

private[fft] final class FFTvsFloatingPointTester(
    dut:         FFT,
    params:      FFTParams,
    check:       FloatingPointCheck,
    pattern:     InputPatterns.FftFramePattern,
    inputFrames: Int,
    tol:         Double,
    plotName:    String,
) extends PeekPokeTester[FFT](dut)
    with PeekPokeDspExtensions {

  private val inFormat       = FFTModel.inputFormat(params)
  private val expectedSamples = ArrayBuffer.empty[Complex]
  private val actualSamples   = ArrayBuffer.empty[Complex]
  private val handshakeRng    = new scala.util.Random(plotName.hashCode.toLong)
  private var compareHeaderPrinted = false

  check match {
    case FirstValidOutputFrame  => testFirstValidOutputFrame()
    case InitialStoring         => testInitialStoring()
    case MultipleAcceptedFrames => testMultipleAcceptedFrames()
  }

  private def testFirstValidOutputFrame(): Unit = {
    require(inputFrames >= 2, "first-output-frame test needs enough input to advance the pipeline")

    val frame    = naturalFrame(frameIndex = 0)
    val input    = FFTModelTestUtils.repeatedDutInput(params, frame, inputFrames)
    val expected = FFTModelTestUtils.floatingPointFrame(params, frame)

    driveAndCompare(input, expected, maxCycles = 80 * params.fftSize * inputFrames)
  }

  private def testMultipleAcceptedFrames(): Unit = {
    require(inputFrames >= 1, "multiple-frame test needs at least one input frame")

    val frames      = Vector.tabulate(inputFrames)(naturalFrame)
    val flushFrames = Vector.fill(4)(Vector.fill(params.fftSize)(RawComplex(0, 0)))
    val input       = FFTModelTestUtils.dutInputFrames(params, frames ++ flushFrames)
    val expected    = frames.flatMap(frame => FFTModelTestUtils.floatingPointFrame(params, frame))

    driveAndCompare(input, expected, maxCycles = 120 * params.fftSize * (inputFrames + flushFrames.length))
  }

  private def testInitialStoring(): Unit = {
    require(
      (params.numAddPipes + params.numMulPipes) != 0,
      "initial-store test requires at least one pipeline register"
    )

    resetFramePorts()

    val frame      = naturalFrame(frameIndex = 0)
    val inputFrame = FFTModelTestUtils.dutInputFrame(params, frame)
    val expected   = FFTModelTestUtils.floatingPointFrame(params, frame)
    val maxCycles  = 80 * params.fftSize

    var firstFrameSample = 0
    var cycles = 0

    poke(dut.io.out.ready, false)
    poke(dut.io.in.valid, true)
    while (firstFrameSample < params.fftSize && cycles < maxCycles) {
      pokeInputSample(inputFrame(firstFrameSample), firstFrameSample)
      if (peek(dut.io.in.ready) == 1) {
        firstFrameSample += 1
      }
      cycles += 1
      step(1)
    }
    assert(firstFrameSample == params.fftSize, "R22 initial-store test did not fill the first input frame")

    var secondFrameSample = 0
    var outputSample = 0
    var lastCount = 0
    poke(dut.io.out.ready, true)

    val cycleLimit = cycleLimitFor(maxCycles)
    while (outputSample < expected.length && cycles < cycleLimit) {
      val driveInput = randomInputValid(secondFrameSample < params.fftSize)
      val outputReady = randomOutputReady
      poke(dut.io.in.valid, driveInput)
      poke(dut.io.out.ready, outputReady)
      if (driveInput) {
        pokeInputSample(inputFrame(secondFrameSample), secondFrameSample)
      } else {
        poke(dut.io.i_last, false)
      }

      if (driveInput && peek(dut.io.in.ready) == 1) {
        secondFrameSample += 1
      }

      if (outputReady && peek(dut.io.out.valid) == 1) {
        val expectedSample = expected(outputSample)
        val actual = peek(dut.io.out.bits)
        compareData(outputSample, expectedSample, actual)
        expectedSamples += expectedSample
        actualSamples += actual
        val expectedLastSample = outputSample == params.fftSize - 1
        assert(
          peek(dut.io.o_last) == expectedLast(expectedLastSample),
          s"R22 initial-store o_last mismatch at output sample $outputSample"
        )
        if (expectedLastSample) {
          lastCount += 1
        }
        outputSample += 1
      }

      cycles += 1
      step(1)
    }

    assert(outputSample == expected.length, s"checked $outputSample of ${expected.length} initial-store output samples")
    assert(lastCount == 1, "R22 initial-store test should see one output frame boundary")
    finishTest()
  }

  private def driveAndCompare(input: Vector[RawComplex], expected: Vector[Complex], maxCycles: Int): Unit = {
    resetFramePorts()

    var inputIndex = 0
    var outputIndex = 0
    var lastCount = 0
    var cycles = 0

    val cycleLimit = cycleLimitFor(maxCycles)
    while (outputIndex < expected.length && cycles < cycleLimit) {
      val driveInput = randomInputValid(inputIndex < input.length)
      val outputReady = randomOutputReady
      poke(dut.io.in.valid, driveInput)
      poke(dut.io.out.ready, outputReady)

      if (driveInput) {
        pokeInputSample(input(inputIndex), inputIndex)
      } else {
        poke(dut.io.i_last, false)
      }

      if (driveInput && peek(dut.io.in.ready) == 1) {
        inputIndex += 1
      }

      if (outputReady && peek(dut.io.out.valid) == 1) {
        val expectedSample = expected(outputIndex)
        val actual = peek(dut.io.out.bits)
        compareData(outputIndex, expectedSample, actual)
        expectedSamples += expectedSample
        actualSamples += actual

        val expectedFrameLast = outputIndex % params.fftSize == params.fftSize - 1
        assert(
          peek(dut.io.o_last) == expectedLast(expectedFrameLast),
          s"o_last mismatch at output sample $outputIndex"
        )
        if (expectedFrameLast) {
          lastCount += 1
        }
        outputIndex += 1
      }

      cycles += 1
      step(1)
    }

    assert(outputIndex == expected.length, s"checked $outputIndex of ${expected.length} floating-point output samples")
    assert(lastCount == expected.length / params.fftSize, "o_last should assert once for each checked output frame")
    finishTest()
  }

  private def naturalFrame(frameIndex: Int): Vector[RawComplex] =
    InputPatterns.fftFrame(params, FFTModelTestUtils.shiftedFramePattern(pattern, frameIndex))

  private def resetFramePorts(): Unit = {
    reset(2)
    poke(dut.io.in.valid, false)
    poke(dut.io.i_last, false)
    poke(dut.io.out.ready, false)
    dut.io.i_load_cfg.foreach(load => poke(load, false))
    step(2)
  }

  private def pokeInputSample(sample: RawComplex, sampleIndex: Int): Unit = {
    poke(dut.io.in.bits, ModelUtils.rawToComplex(inFormat, sample))
    poke(dut.io.i_last, sampleIndex % params.fftSize == params.fftSize - 1)
  }

  private def randomInputValid(hasInput: Boolean): Boolean =
    hasInput && (!TestConfig.randomReadyValid || handshakeRng.nextDouble() < 0.8)

  private def randomOutputReady: Boolean =
    !TestConfig.randomReadyValid || handshakeRng.nextDouble() < 0.8

  private def cycleLimitFor(maxCycles: Int): Int =
    if (TestConfig.randomReadyValid) maxCycles * 4 else maxCycles

  private def compareData(index: Int, expected: Complex, actual: Complex): Unit = {
    val realError = math.abs(expected.real - actual.real)
    val imagError = math.abs(expected.imag - actual.imag)
    logCompareData(index, expected, actual, realError, imagError)
    assert(
      realError <= tol,
      s"[real sample=$index] expected=${expected.real}, actual=${actual.real}, error=$realError, tol=$tol"
    )
    assert(
      imagError <= tol,
      s"[imag sample=$index] expected=${expected.imag}, actual=${actual.imag}, error=$imagError, tol=$tol"
    )
  }

  private def logCompareData(index: Int, expected: Complex, actual: Complex, realError: Double, imagError: Double): Unit =
    if (TestLog.verbose) {
      if (!compareHeaderPrinted) {
        TestLog.log(
          s"\n== FFT vs floating-point ${check.label} ${params.sdfRadix.label} ${params.decimation} " +
            s"size=${params.fftSize} pattern=${pattern.label} =="
        )
        compareHeaderPrinted = true
      }
      TestLog.log(
        f"sample=$index%5d " +
          f"expected=(${expected.real}%.8f, ${expected.imag}%.8f) " +
          f"peeked=(${actual.real}%.8f, ${actual.imag}%.8f) " +
          f"error=($realError%.8f, $imagError%.8f) tol=$tol%.8f"
      )
    }

  private def expectedLast(value: Boolean): BigInt =
    if (value) BigInt(1) else BigInt(0)

  private def finishTest(): Unit = {
    writePlotIfEnabled()
    poke(dut.io.in.valid, false)
    poke(dut.io.i_last, false)
    poke(dut.io.out.ready, false)
    reset(2)
  }

  private def writePlotIfEnabled(): Unit =
    PlotUtils
      .writePlotIfEnabled(plotName, actualSamples.toVector, expectedSamples.toVector, modelLabel = "dut")
      .foreach(output => println(s"wrote FFT-vs-floating-point plot to ${output.getAbsolutePath}"))
}
