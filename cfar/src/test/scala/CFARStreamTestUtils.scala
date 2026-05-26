package opera.cfar

import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import dsptools.numbers._
import fixedpoint._

import scala.util.Random

private[cfar] object CFARStreamTestUtils {
  final case class StreamDut[T <: Data](
      clock : Clock,
      io    : CFARIO[T],
      params: CFARParams[T],
      label : String
  )

  private final case class OutputSnapshot(
    cut      : Option[BigInt],
    threshold: BigInt,
    fftBin   : BigInt,
    peak     : Boolean,
    last     : Boolean
  )

  def pokeCommonControls[T <: Data: Real](
    dut           : StreamDut[T],
    cfarMode      : Int,
    thresholdScale: Double,
    logMode       : Boolean,
    referenceCells: Int,
    guardCells    : Int,
    peakGrouping  : Boolean,
    edgePolicy    : Int,
    fftSize       : Int,
    loadConfig    : Boolean,
    noiseDivShift : Option[Int] = None,
    orderRankLeft : Option[Int] = None,
    orderRankRight: Option[Int] = None
  ): Unit = {
    val activeFftSize = if (fftSize == 0) dut.params.maxFftSize else fftSize

    dut.io.i_fft_size.poke(activeFftSize.U)
    dut.io.i_threshold_scale.poke(CFARModel.literalFor(thresholdScale, dut.params.scaleType))
    dut.io.i_peak_grouping.poke(peakGrouping.B)
    dut.io.i_cfar_mode.poke(cfarMode.U)
    dut.io.i_reference_cells.poke(referenceCells.U)
    dut.io.i_guard_cells.poke(guardCells.U)
    dut.io.i_noise_div_shift.foreach(_.poke(noiseDivShift.getOrElse(log2Ceil(referenceCells)).U))
    dut.io.i_order_rank_left.foreach(_.poke(orderRankLeft.getOrElse(1).U))
    dut.io.i_order_rank_right.foreach(_.poke(orderRankRight.getOrElse(1).U))
    dut.io.i_load_cfg.poke(loadConfig.B)
    dut.io.i_log_mode.foreach(_.poke(logMode.B))
    dut.io.i_edge_policy.foreach(_.poke(edgePolicy.U))
  }

  def expectFrame[T <: Data: Real: BinaryRepresentation](
    dut                   : StreamDut[T],
    frame                 : Seq[Double],
    expectedBins          : Option[Seq[CFARModel.ExpectedBin]],
    readyPattern          : Seq[Boolean],
    randomReadyValidSeed  : Option[Long],
    deterministicMaxCycles: Int,
    randomMaxCycles       : Int,
    onInputAccepted       : Int => Unit
  ): Unit = {
    require(readyPattern.nonEmpty, "readyPattern must not be empty")
    expectedBins.foreach { bins =>
      require(bins.length == frame.length, s"Expected ${frame.length} bins, got ${bins.length}")
    }

    val random = randomReadyValidSeed.map(new Random(_))
    val maxCycles = if (random.isDefined) randomMaxCycles else deterministicMaxCycles

    dut.clock.setTimeout(maxCycles)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(random.map(_ => false).getOrElse(readyPattern.head).B)
    dut.clock.step()
    dut.io.i_load_cfg.poke(false.B)

    var inputIndex = 0
    var outputIndex = 0
    var cycle = 0
    var compareHeaderPrinted = false
    var pendingInput: Option[Int] = None
    var heldOutput: Option[OutputSnapshot] = None

    def mismatchMessage(
      field      : String,
      actualBin  : BigInt,
      expectedRaw: Any,
      actualRaw  : Any,
      trace      : CFARModel.BinTrace
    ): String = {
      s"$field mismatch at cycle=$cycle output=$outputIndex actualBin=$actualBin " +
        s"expected=$expectedRaw actual=$actualRaw trace=$trace"
    }

    while (outputIndex < frame.length && cycle < maxCycles) {
      val outputReady =
        random.map(_.nextInt(4) != 0).getOrElse(readyPattern.lift(cycle).getOrElse(readyPattern.last))
      dut.io.o_data.ready.poke(outputReady.B)

      if (pendingInput.isEmpty && inputIndex < frame.length && random.forall(_.nextInt(5) != 0)) {
        pendingInput = Some(inputIndex)
      }

      pendingInput match {
        case Some(index) =>
          dut.io.i_data.valid.poke(true.B)
          dut.io.i_data.bits.poke(CFARModel.literalFor(frame(index), dut.params.inputType))
          dut.io.i_last.poke((index == frame.length - 1).B)
        case None =>
          dut.io.i_data.valid.poke(false.B)
          dut.io.i_last.poke(false.B)
      }

      val inputFired = pendingInput.nonEmpty && dut.io.i_data.ready.peek().litToBoolean
      val outputValid = dut.io.o_data.valid.peek().litToBoolean
      if (outputValid) {
        val current = outputSnapshot(dut)
        heldOutput.foreach { held =>
          assert(current == held, s"${dut.label} output changed while backpressured: previous=$held current=$current")
        }

        if (outputReady) {
          val expectedBin = expectedBins.map(_(outputIndex))
          val actualBin = dut.io.o_fft_bin.peek().litValue
          val actualLast = dut.io.o_last.peek().litToBoolean
          if (dut.params.sendCut) {
            expectedBin match {
              case Some(bin) =>
                val actualCut = peekRaw(dut.io.o_data.bits.cut.get)
                assert(
                  actualCut == bin.cut.raw,
                  mismatchMessage("CUT", actualBin, bin.cut.raw, actualCut, bin.trace)
                )
              case None =>
                dut.io.o_data.bits.cut.get.expect(CFARModel.literalFor(frame(outputIndex), dut.params.inputType))
            }
          }
          expectedBin.foreach { bin =>
            val actualThreshold = peekRaw(dut.io.o_data.bits.threshold)
            val actualPeak = dut.io.o_data.bits.peak.peek().litToBoolean
            if (TestConfig.verbose) {
              if (!compareHeaderPrinted) {
                println(s"\n== ${dut.label} accepted output samples ==")
                compareHeaderPrinted = true
              }
              println(
                f"cycle=$cycle%4d bin=${actualBin.toInt}%4d cutRaw=${bin.cut.raw}%6d " +
                  f"expectedThresholdRaw=${bin.threshold.raw}%6d actualThresholdRaw=$actualThreshold%6d " +
                  s"expectedPeak=${bin.peak} actualPeak=$actualPeak last=$actualLast " +
                  s"edgePolicy=${bin.trace.edgePolicy}/${bin.trace.edgeBehavior}"
              )
            }
            assert(
              actualThreshold == bin.threshold.raw,
              mismatchMessage("threshold", actualBin, bin.threshold.raw, actualThreshold, bin.trace)
            )
            assert(
              actualPeak == bin.peak,
              mismatchMessage("peak", actualBin, bin.peak, actualPeak, bin.trace)
            )
          }
          assert(
            actualBin == outputIndex,
            expectedBin
              .map(bin => mismatchMessage("FFT bin", actualBin, outputIndex, actualBin, bin.trace))
              .getOrElse(s"FFT bin mismatch at cycle=$cycle output=$outputIndex expected=$outputIndex actual=$actualBin")
          )
          assert(
            actualLast == (outputIndex == frame.length - 1),
            expectedBin
              .map(bin => mismatchMessage("last", actualBin, outputIndex == frame.length - 1, actualLast, bin.trace))
              .getOrElse(s"last mismatch at cycle=$cycle output=$outputIndex expected=${outputIndex == frame.length - 1} actual=$actualLast")
          )
          outputIndex += 1
          heldOutput = None
        } else if (heldOutput.isEmpty) {
          heldOutput = Some(current)
        }
      } else if (heldOutput.nonEmpty) {
        assert(false, s"${dut.label} out.valid dropped while output was backpressured: held=${heldOutput.get}")
        heldOutput = None
      }
      if (inputFired) {
        onInputAccepted(inputIndex)
        inputIndex += 1
        pendingInput = None
      }
      cycle += 1
      dut.clock.step()
      dut.io.i_load_cfg.poke(false.B)
    }
    assert(outputIndex == frame.length, s"Only observed $outputIndex of ${frame.length} output samples")
  }

  def expectOutputStableWhileBackpressured[T <: Data: Real: BinaryRepresentation](
    dut         : StreamDut[T],
    frame       : Seq[Double],
    stableCycles: Int,
    maxCycles   : Int
  ): Unit = {
    require(frame.nonEmpty, s"${dut.label} stability test needs a non-empty frame")

    dut.io.i_data.valid.poke(false.B)
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(false.B)
    dut.clock.step()
    dut.io.i_load_cfg.poke(false.B)

    var inputIndex = 0
    var cycle = 0
    var observedStableCycles = 0
    var previous: Option[OutputSnapshot] = None

    while (observedStableCycles < stableCycles && cycle < maxCycles) {
      if (inputIndex < frame.length) {
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(CFARModel.literalFor(frame(inputIndex), dut.params.inputType))
        dut.io.i_last.poke((inputIndex == frame.length - 1).B)
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }
      dut.io.o_data.ready.poke(false.B)

      val inputFired = dut.io.i_data.valid.peek().litToBoolean && dut.io.i_data.ready.peek().litToBoolean
      if (dut.io.o_data.valid.peek().litToBoolean) {
        val current = outputSnapshot(dut)
        previous.foreach { held =>
          assert(current == held, s"${dut.label} output changed while backpressured: previous=$held current=$current")
        }
        previous = Some(current)
        observedStableCycles += 1
      }
      if (inputFired) inputIndex += 1
      cycle += 1
      dut.clock.step()
    }

    assert(observedStableCycles >= stableCycles, s"Did not observe a backpressured ${dut.label} output payload")
  }

  def expectInputBackpressureWhenFrameBuffersFill[T <: Data: Real: BinaryRepresentation](
    dut      : StreamDut[T],
    frame    : Seq[Double],
    maxCycles: Int
  ): Unit = {
    require(frame.nonEmpty, s"${dut.label} input backpressure test needs a non-empty frame")

    dut.clock.setTimeout(maxCycles)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(false.B)
    dut.clock.step()
    dut.io.i_load_cfg.poke(false.B)

    var inputIndex = 0
    var acceptedInputs = 0
    var cycle = 0
    var observedInputBackpressure = false

    while (!observedInputBackpressure && cycle < maxCycles) {
      dut.io.i_data.valid.poke(true.B)
      dut.io.i_data.bits.poke(CFARModel.literalFor(frame(inputIndex), dut.params.inputType))
      dut.io.i_last.poke((inputIndex == frame.length - 1).B)
      dut.io.o_data.ready.poke(false.B)

      val inputReady = dut.io.i_data.ready.peek().litToBoolean
      if (!inputReady && acceptedInputs >= frame.length) {
        observedInputBackpressure = true
      }
      if (inputReady) {
        inputIndex = (inputIndex + 1) % frame.length
        acceptedInputs += 1
      }

      cycle += 1
      dut.clock.step()
      dut.io.i_load_cfg.poke(false.B)
    }

    assert(observedInputBackpressure, s"i_data.ready did not deassert after $acceptedInputs accepted samples")
    assert(acceptedInputs >= 2 * frame.length, s"Expected both frame buffers to fill before backpressure, accepted=$acceptedInputs")
  }

  private def outputSnapshot[T <: Data: Real](dut: StreamDut[T]): OutputSnapshot =
    OutputSnapshot(
      cut       = dut.io.o_data.bits.cut.map(peekRaw),
      threshold = peekRaw(dut.io.o_data.bits.threshold),
      fftBin    = dut.io.o_fft_bin.peek().litValue,
      peak      = dut.io.o_data.bits.peak.peek().litToBoolean,
      last      = dut.io.o_last.peek().litToBoolean
    )

  private def peekRaw(signal: Data): BigInt = signal match {
    case value: FixedPoint => value.asSInt.peek().litValue
    case value: UInt       => value.peek().litValue
    case value: SInt       => value.peek().litValue
    case other             => throw new IllegalArgumentException(s"Unsupported CFAR debug signal type: ${other.getClass.getName}")
  }
}
