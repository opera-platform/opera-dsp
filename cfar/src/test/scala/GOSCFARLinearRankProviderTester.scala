package opera.cfar

import chisel3._
import chiseltest._
import opera.lis.LISType

private[cfar] object GOSCFARLinearRankProviderTester {
  def providerParams(
      fftSize          : Int,
      maxReferenceCells: Int,
      maxGuardCells    : Int,
      lisType: String = LISType.CntBased
  ): CFARParams[UInt] =
    CFARParams(
      inputType         = UInt(8.W),
      thresholdType     = UInt(8.W),
      scaleType         = UInt(8.W),
      cfarType          = CFARType.OrderedStatistic,
      lisType           = lisType,
      maxFftSize        = fftSize,
      maxReferenceCells = maxReferenceCells,
      maxGuardCells     = maxGuardCells
    )

  def checkWindow(
      dut: GOSCFARLinearRankProvider[UInt],
      frame    : Seq[Int],
      refs     : Int,
      guards   : Int,
      leftRank : Int,
      rightRank: Int,
      bin      : Int
  ): Unit = {
    val span = refs + guards
    dut.io.o_window.bits.fftBin.expect(bin.U)
    dut.io.o_window.bits.cut.expect(frame(bin).U)
    dut.io.o_window.bits.isLeftEdge.expect((bin < span).B)
    dut.io.o_window.bits.isRightEdge.expect((bin >= frame.length - span).B)
    dut.io.o_window.bits.last.expect((bin == frame.length - 1).B)

    if (bin > 0) {
      dut.io.o_window.bits.prev.expect(frame(bin - 1).U)
    }
    if (bin < frame.length - 1) {
      dut.io.o_window.bits.next.expect(frame(bin + 1).U)
    }
    if (bin >= span) {
      dut.io.o_window.bits.leftRank.expect(rank(leftRefs(frame, bin, refs, guards), leftRank).U)
    }
    if (bin < frame.length - span) {
      dut.io.o_window.bits.rightRank.expect(rank(rightRefs(frame, bin, refs, guards), rightRank).U)
    }
  }

  def driveFrame(
      dut: GOSCFARLinearRankProvider[UInt],
      frame       : Seq[Int],
      refs        : Int,
      guards      : Int,
      leftRank    : Int,
      rightRank   : Int,
      readyPattern: Seq[Boolean] = Seq(true)
  )(check: (Int, GOSCFARLinearRankProvider[UInt]) => Unit): Unit = {
    require(readyPattern.nonEmpty, "readyPattern must not be empty")

    dut.clock.setTimeout(frame.length * 10)
    dut.io.i_fft_size.poke(frame.length.U)
    dut.io.i_reference_cells.poke(refs.U)
    dut.io.i_guard_cells.poke(guards.U)
    dut.io.i_order_rank_left.poke(leftRank.U)
    dut.io.i_order_rank_right.poke(rightRank.U)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_data.bits.poke(0.U)
    dut.io.i_last.poke(false.B)
    dut.io.i_output_done.poke(false.B)
    dut.io.o_window.ready.poke(readyPattern.head.B)
    dut.clock.step()

    var inputIndex = 0
    var outputIndex = 0
    var cycle = 0
    var heldPayload: Option[Seq[BigInt]] = None

    while (outputIndex < frame.length && cycle < frame.length * 10) {
      val outputReady = readyPattern.lift(cycle).getOrElse(readyPattern.last)
      dut.io.o_window.ready.poke(outputReady.B)
      dut.io.i_output_done.poke(false.B)

      if (inputIndex < frame.length) {
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(frame(inputIndex).U)
        dut.io.i_last.poke((inputIndex == frame.length - 1).B)
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }
      val inputFired = dut.io.i_data.valid.peek().litToBoolean && dut.io.i_data.ready.peek().litToBoolean

      if (dut.io.o_window.valid.peek().litToBoolean) {
        val current = payloadSnapshot(dut)
        if (outputReady) {
          heldPayload.foreach(held => assert(current == held, "provider payload changed before the held payload fired"))
          heldPayload = None
          check(outputIndex, dut)
          if (outputIndex == frame.length - 1) {
            dut.io.i_output_done.poke(true.B)
          }
          outputIndex += 1
        } else {
          heldPayload.foreach(held => assert(current == held, "provider payload changed while backpressured"))
          if (heldPayload.isEmpty) {
            heldPayload = Some(current)
          }
        }
      } else if (heldPayload.nonEmpty) {
        assert(false, s"provider valid dropped while output was backpressured: held=${heldPayload.get}")
      }

      if (inputFired) {
        inputIndex += 1
      }

      cycle += 1
      dut.clock.step()
    }

    assert(outputIndex == frame.length, s"observed $outputIndex of ${frame.length} rank windows")
  }

  def driveLastAlignmentViolation(
      dut      : GOSCFARLinearRankProvider[UInt],
      frameSize: Int,
      lastIndex: Int,
      maxInputs: Int
  ): Unit = {
    dut.io.i_fft_size.poke(frameSize.U)
    dut.io.i_reference_cells.poke(2.U)
    dut.io.i_guard_cells.poke(1.U)
    dut.io.i_order_rank_left.poke(1.U)
    dut.io.i_order_rank_right.poke(1.U)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_data.bits.poke(0.U)
    dut.io.i_last.poke(false.B)
    dut.io.i_output_done.poke(false.B)
    dut.io.o_window.ready.poke(true.B)
    dut.clock.step()

    var inputIndex = 0
    var cycle = 0
    while (cycle < frameSize * 5) {
      dut.io.o_window.ready.poke(true.B)
      dut.io.i_output_done.poke(false.B)
      if (dut.io.o_window.valid.peek().litToBoolean && dut.io.o_window.bits.last.peek().litToBoolean) {
        dut.io.i_output_done.poke(true.B)
      }

      if (inputIndex < maxInputs && dut.io.i_data.ready.peek().litToBoolean) {
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(inputIndex.U)
        dut.io.i_last.poke((inputIndex == lastIndex).B)
        inputIndex += 1
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }

      cycle += 1
      dut.clock.step()
    }
  }

  private def rank(values: Seq[Int], oneBasedRank: Int): BigInt = BigInt(values.sorted.apply(oneBasedRank - 1))

  private def leftRefs(frame: Seq[Int], bin: Int, refs: Int, guards: Int): Seq[Int]  = frame.slice(bin - guards - refs, bin - guards)
  private def rightRefs(frame: Seq[Int], bin: Int, refs: Int, guards: Int): Seq[Int] = frame.slice(bin + guards + 1, bin + guards + 1 + refs)

  private def payloadSnapshot(dut: GOSCFARLinearRankProvider[UInt]): Seq[BigInt] =
    Seq(
      dut.io.o_window.bits.fftBin.peek().litValue,
      dut.io.o_window.bits.cut.peek().litValue,
      dut.io.o_window.bits.leftRank.peek().litValue,
      dut.io.o_window.bits.rightRank.peek().litValue,
      dut.io.o_window.bits.prev.peek().litValue,
      dut.io.o_window.bits.next.peek().litValue,
      BigInt(if (dut.io.o_window.bits.isLeftEdge.peek().litToBoolean) 1 else 0),
      BigInt(if (dut.io.o_window.bits.isRightEdge.peek().litToBoolean) 1 else 0),
      BigInt(if (dut.io.o_window.bits.last.peek().litToBoolean) 1 else 0)
    )
}
