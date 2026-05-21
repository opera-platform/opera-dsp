package opera.fft

import breeze.math.Complex
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions

import scala.collection.mutable.ArrayBuffer

/**
 * Drives the FFT DUT and plots SQNR against a floating-point FFT reference.
 */
private[fft] final class FFTSQNRTester(
    dut     : FFT,
    params  : FFTParams,
    pattern : InputPatterns.FftFramePattern,
    plotName: String,
) extends PeekPokeTester[FFT](dut)
    with PeekPokeDspExtensions {

  private val inFormat  = FFTModel.inputFormat(params)
  private val peekedData = ArrayBuffer.empty[Complex]

  private val targetFrame     = InputPatterns.fftFrame(params, pattern)
  private val inputStreamData = FFTModelTestUtils.repeatedDutInput(params, targetFrame, frames = 3)
  private val expectedData    = FFTModelTestUtils.floatingPointFrame(params, targetFrame)

  reset(2)
  poke(dut.io.in.valid, false)
  poke(dut.io.i_last, false)
  poke(dut.io.out.ready, false)
  dut.io.i_load_cfg.foreach(load => poke(load, false))
  step(2)

  var writeCounter = 0
  var readCounter  = 0
  var cycles       = 0
  val maxCycles    = 120 * inputStreamData.length

  while (readCounter < expectedData.length && cycles < maxCycles) {
    val hasInput = writeCounter < inputStreamData.length
    poke(dut.io.in.valid, hasInput)
    poke(dut.io.out.ready, true)

    if (hasInput) {
      poke(dut.io.in.bits, ModelUtils.rawToComplex(inFormat, inputStreamData(writeCounter)))
      poke(dut.io.i_last, writeCounter % params.fftSize == params.fftSize - 1)
    } else {
      poke(dut.io.i_last, false)
    }

    if (hasInput && peek(dut.io.in.ready) == 1) writeCounter += 1

    if (peek(dut.io.out.valid) == 1 && peek(dut.io.out.ready) == 1) {
      val expectedLast = if (readCounter == expectedData.length - 1) BigInt(1) else BigInt(0)
      val peekedLast = peek(dut.io.o_last)
      assert(
        peekedLast == expectedLast,
        s"SQNR o_last mismatch at sample $readCounter: expected last=$expectedLast peeked last=$peekedLast"
      )
      peekedData += peek(dut.io.out.bits)
      readCounter += 1
    }

    cycles += 1
    step(1)
  }

  assert(readCounter == expectedData.length, s"checked $readCounter of ${expectedData.length} SQNR output samples")

  val signalPower = power(expectedData)
  val noisePower = power(
    peekedData.zip(expectedData).map { case (peekedSample, expectedSample) => peekedSample - expectedSample }
  )
  val sqnr =
    if (noisePower == 0.0) 300.0
    else if (signalPower == 0.0) -300.0
    else 10.0 * math.log10(signalPower / noisePower)

  TestLog.log(f"$plotName SQNR=$sqnr%.2f dB")
  PlotUtils
    .writeSqnrPlotIfEnabled(plotName, sqnr)
    .foreach(output => println(s"wrote DUT SQNR plot to ${output.getAbsolutePath}"))

  private def power(samples: Iterable[Complex]): Double =
    samples.map(sample => sample.real * sample.real + sample.imag * sample.imag).sum
}
