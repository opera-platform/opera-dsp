package opera.fft

import breeze.math.Complex
import chisel3.{Bits, Module}
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions
import ModelUtils.{FixedFormat, RawComplex}

/**
 * Checks that an idle SDF stage does not advance counters or assert valid output while enable is low.
 */
class SDFStageIdleDutTester[DUT <: Module](
    dut            : DUT,
    io             : RadixIO,
    params         : RadixParams,
    expectedCounter: Int = 0,
) extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {

  reset(2)
  poke(io.i_en, false)
  poke(io.in, Complex(0.0, 0.0))
  io.i_divBy2.foreach(div => poke(div, false))

  for (cycle <- 0 until (params.stageSize + params.latency + params.addPipeRegs + 4)) {
    assert(peek(io.o_counter) == expectedCounter, s"idle counter moved at cycle $cycle")
    assert(peek(io.o_en) == 0, s"idle o_en asserted at cycle $cycle")
    step(1)
  }
}

/**
 * Checks one SDF stage against the scalar model by driving deterministic raw patterns, optional stalls,
 * runtime div-by-2 controls, and overflow stress while matching counters, valid flags, and raw outputs.
 */
class SDFStageDutTester[DUT <: Module](
    dut           : DUT,
    io            : RadixIO,
    params        : RadixParams,
    model         : FFTStageModel,
    debugName     : String,
    inputPattern  : (FixedFormat, Int) => Vector[RawComplex],
    divBy2RegValue: Boolean = false,
    requireOverflowCoverage: Boolean = false,
    divBy2RegControl:        Option[Int => Boolean] = None,
) extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {
  import FFTStageModel.StageTrace

  private val inputs = inputPattern(model.inputFormat, params.stageSize)
  require(inputs.nonEmpty, s"$debugName stage tester requires at least one input sample")

  private def inputAt(cycle: Int): RawComplex =
    inputs(cycle % inputs.length)

  private def enabledAt(cycle: Int): Boolean =
    cycle < warmupCycles || (cycle % 7 != 3 && cycle % 11 != 5)

  private def divBy2At(cycle: Int): Boolean =
    if (params.divBy2Reg) divBy2RegControl.map(_(cycle)).getOrElse(divBy2RegValue) else params.divBy2

  private def noteOverflowCoverage(trace: StageTrace): Unit = {
    Seq(
      trace.sum.real  -> true,
      trace.diff.real -> true,
      trace.sum.imag  -> false,
      trace.diff.imag -> false,
    ).foreach { case (raw, isRealLane) =>
      if (raw > model.inputFormat.maxRaw) {
        sawPositiveOverflow = true
        if (isRealLane) sawRealLaneOverflow = true else sawImagLaneOverflow = true
      }
      if (raw < model.inputFormat.minRaw) {
        sawNegativeOverflow = true
        if (isRealLane) sawRealLaneOverflow = true else sawImagLaneOverflow = true
      }
    }
  }

  private val warmupCycles = 3 * params.stageSize + params.latency + params.addPipeRegs + params.delay + 8
  private val targetValidOutputs = inputs.length
  private val maxCycles = warmupCycles + 20 * targetValidOutputs + params.stageSize + params.latency + params.addPipeRegs + params.delay + 32
  private var compareHeaderPrinted = false

  private def printCompareHeaderOnce(): Unit = {
    if (TestLog.verbose && !compareHeaderPrinted) {
      TestLog.printStageCompareHeader(s"$debugName ${params.decimation} stageSize=${params.stageSize} output samples")
      compareHeaderPrinted = true
    }
  }

  reset(2)
  model.reset()
  poke(io.i_en, false)
  poke(io.in, Complex(0.0, 0.0))
  io.i_divBy2.foreach(div => poke(div, divBy2At(0)))
  step(1)

  var checkedValidOutputs = 0
  var sawExpectedOverflow = false
  var sawPositiveOverflow = false
  var sawNegativeOverflow = false
  var sawRealLaneOverflow = false
  var sawImagLaneOverflow = false
  var cycle = 0
  while (checkedValidOutputs < targetValidOutputs && cycle < maxCycles) {
    val input = inputAt(cycle)
    val enable = enabledAt(cycle)
    val divBy2 = divBy2At(cycle)
    val expected = model.step(input, enable, divBy2)
    noteOverflowCoverage(expected.trace)

    poke(io.i_en, enable)
    poke(
      io.in,
      Complex(model.inputFormat.toDouble(input.real), model.inputFormat.toDouble(input.imag))
    )
    io.i_divBy2.foreach(div => poke(div, divBy2))

    assert(peek(io.o_counter) == expected.counter, s"counter mismatch at cycle $cycle")
    assert(peek(io.o_en) == (if (expected.valid) 1 else 0), s"o_en mismatch at cycle $cycle")
    io.o_overflow.foreach { overflow =>
      assert(
        peek(overflow) == (if (expected.overflow) 1 else 0),
        s"overflow mismatch at cycle $cycle: expected ${expected.overflow}"
      )
    }
    sawExpectedOverflow ||= expected.overflow

    if (expected.valid) {
      val received = peek(io.out)
      val receivedRawReal = model.outputFormat.wrap(peek(io.out.real.asSInt.asInstanceOf[Bits]))
      val receivedRawImag = model.outputFormat.wrap(peek(io.out.imag.asSInt.asInstanceOf[Bits]))
      val expectedComplex = Complex(
        model.outputFormat.toDouble(expected.output.real),
        model.outputFormat.toDouble(expected.output.imag)
      )
      printCompareHeaderOnce()
      TestLog.printStageCompareRow(
        cycle,
        expected.counter,
        enable,
        divBy2,
        expected.output.real,
        expected.output.imag,
        receivedRawReal,
        receivedRawImag,
        expectedComplex,
        received
      )
      assert(
        receivedRawReal == expected.output.real,
        s"real mismatch at cycle $cycle: expected raw ${expected.output.real}, received raw $receivedRawReal (${received.real}), trace=${expected.trace}"
      )
      assert(
        receivedRawImag == expected.output.imag,
        s"imag mismatch at cycle $cycle: expected raw ${expected.output.imag}, received raw $receivedRawImag (${received.imag}), trace=${expected.trace}"
      )
      checkedValidOutputs += 1
    }

    step(1)
    cycle += 1
  }

  assert(
    checkedValidOutputs == targetValidOutputs,
    s"test checked $checkedValidOutputs of $targetValidOutputs valid stage outputs before maxCycles=$maxCycles"
  )
  if (requireOverflowCoverage) {
    assert(sawExpectedOverflow, "overflow stress test did not observe an expected overflow")
    assert(sawPositiveOverflow, "overflow stress test did not exercise positive overflow")
    assert(sawNegativeOverflow, "overflow stress test did not exercise negative overflow")
    assert(sawRealLaneOverflow, "overflow stress test did not exercise real-lane overflow")
    assert(sawImagLaneOverflow, "overflow stress test did not exercise imag-lane overflow")
  }
}

/**
 * Resets an SDF stage between independent input bursts and checks every valid
 * sample from each post-reset epoch against the scalar model.
 */
class SDFStageResetDutTester[DUT <: Module](
    dut      : DUT,
    io       : RadixIO,
    params   : RadixParams,
    model    : FFTStageModel,
    debugName: String,
) extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {

  private val chirpsPerEpoch = 4
  private val resetEpochs = 3
  private val checkedSamplesPerEpoch = chirpsPerEpoch * params.stageSize - params.delay
  private val warmupSamplesPerEpoch = 0
  private val maxCyclesPerEpoch = 20 * params.stageSize + params.latency + params.addPipeRegs + params.delay + 32
  private val zero = RawComplex(0, 0)

  private def chirp(epoch: Int, chirpIndex: Int): Vector[RawComplex] = {
    val amplitude = (model.inputFormat.maxRaw / 16).max(1)
    Vector.tabulate(params.stageSize) { sample =>
      val phase = 2.0 * math.Pi * (sample * sample + (epoch + 1) * (chirpIndex + 2) * sample).toDouble / params.stageSize.toDouble
      RawComplex(
        model.inputFormat.wrap(BigDecimal(math.cos(phase) * amplitude.toDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt),
        model.inputFormat.wrap(BigDecimal(math.sin(phase) * amplitude.toDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt)
      )
    }
  }

  private def epochInput(epoch: Int): Vector[RawComplex] =
    Vector.tabulate(chirpsPerEpoch)(chirpIndex => chirp(epoch, chirpIndex)).flatten

  private def initializePins(): Unit = {
    poke(io.i_en, false)
    poke(io.in, Complex(0.0, 0.0))
    io.i_divBy2.foreach(div => poke(div, params.divBy2))
  }

  reset(2)
  initializePins()
  model.reset()
  model.stepReset(zero, params.divBy2)
  model.stepReset(zero, params.divBy2)
  step(1)
  model.step(zero, enable = false, params.divBy2)

  for (epoch <- 0 until resetEpochs) {
    if (epoch > 0) {
      initializePins()
      model.reset()
      reset(2)
      model.stepReset(zero, params.divBy2)
      model.stepReset(zero, params.divBy2)
      initializePins()
      step(1)
      model.step(zero, enable = false, params.divBy2)
    }

    val input = epochInput(epoch)
    var inputIndex = 0
    var validIndex = 0
    var checked = 0
    var cycles = 0

    while (checked < checkedSamplesPerEpoch && cycles < maxCyclesPerEpoch) {
      val hasInput = inputIndex < input.length
      val sample = if (hasInput) input(inputIndex) else zero
      val expected = model.step(sample, hasInput, params.divBy2)

      poke(io.i_en, hasInput)
      poke(
        io.in,
        Complex(model.inputFormat.toDouble(sample.real), model.inputFormat.toDouble(sample.imag))
      )
      io.i_divBy2.foreach(div => poke(div, params.divBy2))

      assert(
        peek(io.o_counter) == expected.counter,
        s"$debugName counter mismatch after reset epoch $epoch cycle $cycles"
      )
      assert(
        peek(io.o_en) == (if (expected.valid) 1 else 0),
        s"$debugName o_en mismatch after reset epoch $epoch cycle $cycles"
      )

      if (expected.valid) {
        if (validIndex >= warmupSamplesPerEpoch && checked < checkedSamplesPerEpoch) {
          val receivedRawReal = model.outputFormat.wrap(peek(io.out.real.asSInt.asInstanceOf[Bits]))
          val receivedRawImag = model.outputFormat.wrap(peek(io.out.imag.asSInt.asInstanceOf[Bits]))
          assert(
            receivedRawReal == expected.output.real,
            s"$debugName real mismatch after reset epoch $epoch sample $checked cycle $cycles: " +
              s"expected raw ${expected.output.real}, received raw $receivedRawReal, trace=${expected.trace}"
          )
          assert(
            receivedRawImag == expected.output.imag,
            s"$debugName imag mismatch after reset epoch $epoch sample $checked cycle $cycles: " +
              s"expected raw ${expected.output.imag}, received raw $receivedRawImag, trace=${expected.trace}"
          )
          checked += 1
        }
        validIndex += 1
      }

      if (hasInput) {
        inputIndex += 1
      }
      cycles += 1
      step(1)
    }

    assert(
      checked == checkedSamplesPerEpoch,
      s"$debugName reset epoch $epoch checked $checked of $checkedSamplesPerEpoch expected outputs"
    )
  }

  initializePins()
}
