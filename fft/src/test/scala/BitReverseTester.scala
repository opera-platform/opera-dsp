package opera.fft

import chiseltest.iotesters.PeekPokeTester
import fixedpoint._

/**
 * Checks BitReverse ordering by sending several deterministic frames with periodic input/output stalls
 * and comparing each accepted output sample against the shared bit-reversal helper.
 */
class BitReverseTester(
    dut      : BitReverse,
    frameSize: Int,
) extends PeekPokeTester(dut) {
  require(frameSize > 1 && (frameSize & (frameSize - 1)) == 0, "frameSize must be a power of two")
  require(frameSize <= dut.params.memDepth, "frameSize must fit within BitReverse memDepth")

  private val frames = Seq(
    Seq.tabulate(frameSize)(i => BigInt(i)),
    Seq.tabulate(frameSize)(i => BigInt(i + frameSize)),
    Seq.tabulate(frameSize)(i => BigInt(i + 1)),
    Seq.tabulate(frameSize)(i => BigInt(i + frameSize + 1)),
  )
  private val input = frames.flatten
  private val expected = frames.flatMap(TestUtils.bitReverse)
  private val maxCycles = 32 * dut.params.memDepth

  dut.io.i_samples.foreach(samples => poke(samples, frameSize))
  reset(2)
  dut.io.i_samples.foreach(samples => poke(samples, frameSize))
  poke(dut.io.in.valid, false)
  poke(dut.io.i_last, false)
  poke(dut.io.out.ready, false)
  step(2)

  var written = 0
  var read = 0
  var cycle = 0

  while (read < expected.length && cycle < maxCycles) {
    val driveInput = written < input.length && cycle % 5 != 1
    val outputReady = cycle % 4 != 2

    poke(dut.io.in.valid, driveInput)
    poke(dut.io.out.ready, outputReady)
    if (driveInput) {
      val sample = input(written)
      poke(dut.io.in.bits.real.asSInt, sample)
      poke(dut.io.in.bits.imag.asSInt, sample)
      poke(dut.io.i_last, (written % frameSize) == frameSize - 1)
    } else {
      poke(dut.io.i_last, false)
    }

    if (driveInput && peek(dut.io.in.ready) == 1) {
      written += 1
    }

    if (outputReady && peek(dut.io.out.valid) == 1) {
      val actual = peek(dut.io.out.bits.real.asSInt)
      val actualLast = peek(dut.io.o_last)
      val expectedValue = expected(read)
      val expectedLast = if (read % frameSize == frameSize - 1) BigInt(1) else BigInt(0)

      assert(actual == expectedValue, s"BitReverse data mismatch at sample $read: expected=$expectedValue actual=$actual")
      assert(actualLast == expectedLast, s"BitReverse last mismatch at sample $read: expected=$expectedLast actual=$actualLast")
      read += 1
    }

    cycle += 1
    step(1)
  }

  assert(written == input.length, s"BitReverse accepted $written of ${input.length} input samples")
  assert(read == expected.length, s"BitReverse produced $read of ${expected.length} output samples")
  TestLog.log(s"BitReverse checked $read samples with frameSize=$frameSize")
}
