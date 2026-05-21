package opera.fft

import breeze.math.Complex
import chisel3.Bits
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions
import fixedpoint.FixedPoint
import scala.collection.mutable.ArrayBuffer
import ModelUtils.RawComplex

private abstract class DirectFFTCoreTester[DUT <: HasIO](dut: DUT, protected val params: FFTParams)
    extends PeekPokeTester[DUT](dut)
    with PeekPokeDspExtensions {
  protected val inFormat  = FFTModel.inputFormat(params)
  protected val outFormat = FFTModel.fftOutputFormat(params)

  /**
   * Converts one raw input sample into the real-valued representation expected by
   * the PeekPoke DspComplex helpers.
   */
  protected def toComplex(sample: RawComplex): Complex =
    Complex(inFormat.toDouble(sample.real), inFormat.toDouble(sample.imag))

  /**
   * Reads one FixedPoint output lane and wraps it into the modeled output format.
   */
  protected def peekRaw(signal: FixedPoint): BigInt =
    outFormat.wrap(peek(signal.asSInt.asInstanceOf[Bits]))

  /**
   * Reads the current complex DUT output sample as raw signed FixedPoint bits.
   */
  protected def peekOutputRaw(): RawComplex =
    RawComplex(peekRaw(dut.io.out.bits.real), peekRaw(dut.io.out.bits.imag))

  /**
   * Expected frame-boundary marker for a checked output sample index.
   */
  protected def expectedLast(index: Int): BigInt =
    if (index % params.fftSize == params.fftSize - 1) BigInt(1) else BigInt(0)

  protected def driveInput(input: Vector[RawComplex], index: Int, valid: Boolean): Unit = {
    poke(dut.io.in.valid, valid)
    if (valid) {
      poke(dut.io.in.bits, toComplex(input(index)))
      poke(dut.io.i_last, index % params.fftSize == params.fftSize - 1)
    } else {
      poke(dut.io.i_last, false)
    }
  }

  protected def driveInput(input: Vector[RawComplex], index: Int): Unit =
    driveInput(input, index, index < input.length)

  protected def resetPorts(outReady: Boolean): Unit = {
    reset(2)
    poke(dut.io.in.valid, false)
    poke(dut.io.i_last, false)
    poke(dut.io.out.ready, outReady)
    dut.io.i_load_cfg.foreach(load => poke(load, false))
    step(2)
  }
}

/**
 * Drives an RTL FFT core and checks its raw output stream against expected model samples.
 *
 * The tester treats the `expected` vector as the reference output produced by
 * `FFTModel`, then drives the given raw input samples through a direct FFT core
 * DUT. Each valid DUT output is converted back to raw signed bits and compared
 * sample-by-sample. It also checks `o_last`, because frame boundaries are part of
 * the streaming contract.
 *
 * The spec owns model execution and passes the expected samples here, keeping the
 * driver focused on ready/valid IO and raw-bit comparison.
 *
 * @param dut      Direct FFT core under test, such as `R2FFT` or `R22FFT`.
 * @param params   FFT parameters used to interpret input and output raw formats.
 * @param input    Raw input stream samples to drive into the DUT.
 * @param expected Raw output samples expected from the DUT in stream order.
 * @param plotName Stable plot filename stem used when `fft.plot=true`.
 */
private final class FFTvsModelTester[DUT <: HasIO](
    dut     : DUT,
    params  : FFTParams,
    input   : Vector[RawComplex],
    expected: Vector[RawComplex],
    plotName: String,
) extends DirectFFTCoreTester[DUT](dut, params) {
  private val checkedActual = ArrayBuffer.empty[RawComplex]
  private val handshakeRng  = new scala.util.Random(plotName.hashCode.toLong)

  private def randomInputValid(hasInput: Boolean): Boolean =
    hasInput && (!TestConfig.randomReadyValid || handshakeRng.nextDouble() < 0.8)

  private def randomOutputReady: Boolean =
    !TestConfig.randomReadyValid || handshakeRng.nextDouble() < 0.8

  /**
   * Writes a DUT-vs-model magnitude/error plot when plotting is enabled.
   */
  private def writePlotIfEnabled(): Unit =
    if (checkedActual.nonEmpty) {
      val actual = checkedActual.toVector.map(ModelUtils.rawToComplex(outFormat, _))
      val model  = expected.take(checkedActual.length).map(ModelUtils.rawToComplex(outFormat, _))
      PlotUtils
        .writePlotIfEnabled(plotName, actual, model, modelLabel = "dut", breezeLabel = "model")
        .foreach(output => println(s"wrote DUT-vs-model FFT plot to ${output.getAbsolutePath}"))
    }

  resetPorts(outReady = true)

  var inputIndex  = 0
  var outputIndex = 0
  var cycle       = 0
  val maxCycles   = 64 * params.fftSize
  val cycleLimit  = if (TestConfig.randomReadyValid) maxCycles * 4 else maxCycles

  while (outputIndex < expected.length && cycle < cycleLimit) {
    val inputValid  = randomInputValid(inputIndex < input.length)
    val outputReady = randomOutputReady
    driveInput(input, inputIndex, inputValid)
    poke(dut.io.out.ready, outputReady)

    if (inputValid && peek(dut.io.in.ready) == 1) {
      inputIndex += 1
    }

    if (outputReady && peek(dut.io.out.valid) == 1) {
      val actual = peekOutputRaw()
      val actualLast = peek(dut.io.o_last)
      val expectedSample = expected(outputIndex)

      assert(
        actual == expectedSample,
        s"output mismatch at sample $outputIndex: expected=$expectedSample actual=$actual"
      )
      assert(
        actualLast == expectedLast(outputIndex),
        s"o_last mismatch at sample $outputIndex: expected=${expectedLast(outputIndex)} actual=$actualLast"
      )
      checkedActual += actual
      outputIndex += 1
    }

    cycle += 1
    step(1)
  }

  assert(outputIndex == expected.length, s"checked $outputIndex of ${expected.length} output samples")
  writePlotIfEnabled()
}

/**
 * Holds the direct FFT output unready until the shallow queue backpressures input,
 * then drains and checks that the first output frame is still intact.
 */
private final class FFTOutputStallTester[DUT <: HasIO](
    dut     : DUT,
    params  : FFTParams,
    input   : Vector[RawComplex],
    expected: Vector[RawComplex],
    label   : String,
) extends DirectFFTCoreTester[DUT](dut, params) {
  resetPorts(outReady = false)

  var inputIndex = 0
  var cycles     = 0
  var sawInputBackpressure = false
  var heldOutput: Option[(RawComplex, BigInt)] = None
  val stallCycleLimit = 128 * params.fftSize + 512

  while (!sawInputBackpressure && cycles < stallCycleLimit) {
    poke(dut.io.out.ready, false)
    driveInput(input, inputIndex)

    if (peek(dut.io.out.valid) == 1) {
      val current = (peekOutputRaw(), peek(dut.io.o_last))
      heldOutput match {
        case Some(held) =>
          assert(current == held, s"$label output changed while stalled: held=$held current=$current")
        case None =>
          heldOutput = Some(current)
      }
    }

    if (inputIndex < input.length && peek(dut.io.in.ready) == 1) {
      inputIndex += 1
    } else if (inputIndex < input.length && peek(dut.io.in.ready) == 0) {
      sawInputBackpressure = true
    }

    cycles += 1
    step(1)
  }

  assert(sawInputBackpressure, s"$label did not backpressure input under output stall")
  assert(heldOutput.nonEmpty, s"$label backpressured input without presenting a held output")

  var outputIndex = 0
  val drainCycleLimit = cycles + 128 * params.fftSize + input.length * 4 + 512
  while (outputIndex < expected.length && cycles < drainCycleLimit) {
    poke(dut.io.out.ready, true)
    driveInput(input, inputIndex)

    if (inputIndex < input.length && peek(dut.io.in.ready) == 1) {
      inputIndex += 1
    }

    if (peek(dut.io.out.valid) == 1) {
      val actual = peekOutputRaw()
      val actualLast = peek(dut.io.o_last)
      val expectedSample = expected(outputIndex)
      assert(actual == expectedSample, s"$label output mismatch at sample $outputIndex: expected=$expectedSample actual=$actual")
      assert(
        actualLast == expectedLast(outputIndex),
        s"$label o_last mismatch at sample $outputIndex: expected=${expectedLast(outputIndex)} actual=$actualLast"
      )
      outputIndex += 1
    }

    cycles += 1
    step(1)
  }

  assert(outputIndex == expected.length, s"$label drained $outputIndex of ${expected.length} expected output samples")
}
