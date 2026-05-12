package opera.fft

import breeze.math.Complex
import chisel3.Bits
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions
import fixedpoint.FixedPoint
import java.io.File
import scala.collection.mutable.ArrayBuffer
import ModelUtils.RawComplex

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
    dut:      DUT,
    params:   FFTParams,
    input:    Vector[RawComplex],
    expected: Vector[RawComplex],
    plotName: String,
) extends PeekPokeTester[DUT](dut)
    with PeekPokeDspExtensions {

  private val inFormat      = FFTModel.inputFormat(params)
  private val outFormat     = FFTModel.fftOutputFormat(params)
  private val checkedActual = ArrayBuffer.empty[RawComplex]

  /**
   * Converts one raw input sample into the real-valued representation expected by
   * the PeekPoke DspComplex helpers.
   */
  private def toComplex(sample: RawComplex): Complex = Complex(inFormat.toDouble(sample.real), inFormat.toDouble(sample.imag))

  /**
   * Reads one FixedPoint output lane and wraps it into the modeled output format.
   */
  private def peekRaw(signal: FixedPoint): BigInt = outFormat.wrap(peek(signal.asSInt.asInstanceOf[Bits]))

  /**
   * Reads the current complex DUT output sample as raw signed FixedPoint bits.
   */
  private def peekOutputRaw(): RawComplex = RawComplex(peekRaw(dut.io.out.bits.real), peekRaw(dut.io.out.bits.imag))

  /**
   * Expected frame-boundary marker for a checked output sample index.
   */
  private def expectedLast(index: Int): BigInt = if (index % params.fftSize == params.fftSize - 1) BigInt(1) else BigInt(0)

  /**
   * Writes a DUT-vs-model magnitude/error plot when plotting is enabled.
   */
  private def writePlotIfEnabled(): Unit =
    if (TestConfig.plot) {
      val safeName = plotName.replace("^", "x").replaceAll("[^A-Za-z0-9_.-]", "-")
      val actual = checkedActual.toVector.map(ModelUtils.rawToComplex(outFormat, _))
      val model  = expected.take(checkedActual.length).map(ModelUtils.rawToComplex(outFormat, _))
      val output = PlotUtils.writePlot(
        output      = new File(TestConfig.plotDirectory, s"$safeName.png"),
        title       = plotName,
        model       = actual,
        breeze      = model,
        modelLabel  = "dut",
        breezeLabel = "model"
      )
      println(s"wrote DUT-vs-model FFT plot to ${output.getAbsolutePath}")
    }

  reset(2)
  poke(dut.io.in.valid, false)
  poke(dut.io.i_last, false)
  poke(dut.io.out.ready, true)
  dut.io.i_load_cfg.foreach(load => poke(load, false))
  step(2)

  var inputIndex = 0
  var outputIndex = 0
  var cycle = 0
  val maxCycles = 64 * params.fftSize

  while (outputIndex < expected.length && cycle < maxCycles) {
    val driveInput = inputIndex < input.length
    poke(dut.io.in.valid, driveInput)
    poke(dut.io.out.ready, true)

    if (driveInput) {
      poke(dut.io.in.bits, toComplex(input(inputIndex)))
      poke(dut.io.i_last, inputIndex % params.fftSize == params.fftSize - 1)
    } else {
      poke(dut.io.i_last, false)
    }

    if (driveInput && peek(dut.io.in.ready) == 1) {
      inputIndex += 1
    }

    if (peek(dut.io.out.valid) == 1) {
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
