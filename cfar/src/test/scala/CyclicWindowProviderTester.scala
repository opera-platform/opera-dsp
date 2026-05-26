package opera.cfar

import chisel3._
import chiseltest._

private[cfar] object CyclicWindowProviderTester {
  def paramsForReplay(
      size             : Int,
      maxReferenceCells: Int,
      maxGuardCells    : Int
  ): CFARParams[UInt] =
    CFARParams(
      inputType         = UInt(16.W),
      thresholdType     = UInt(16.W),
      scaleType         = UInt(16.W),
      maxFftSize        = size,
      maxReferenceCells = maxReferenceCells,
      maxGuardCells     = maxGuardCells,
      edgePolicy        = CFAREdgePolicy.WrapAroundFrame
    )

  def driveReplayAndCheck(
      dut            : CyclicWindowProvider[UInt],
      size           : Int,
      referenceCells : Int,
      guardCells     : Int
  ): Unit = {
    val replay = replayAddrs(size, referenceCells, guardCells)

    dut.io.i_cfg.fft_size.poke(size.U)
    dut.io.i_cfg.reference_cells.poke(referenceCells.U)
    dut.io.i_cfg.guard_cells.poke(guardCells.U)
    dut.io.i_cfg.noise_div_shift.poke(0.U)
    dut.io.i_cfg.order_rank_left.poke(1.U)
    dut.io.i_cfg.order_rank_right.poke(1.U)
    dut.io.i_cfg.cfar_mode.poke(CFARMode.CellAveraging.U)
    dut.io.i_cfg.edge_policy.poke(CFAREdgePolicy.WrapAroundFrame.U)
    dut.io.i_cfg.peak_grouping.poke(false.B)
    dut.io.i_cfg.threshold_scale.poke(0.U)
    dut.io.i_cfg.log_mode.poke(false.B)

    dut.io.i_data.valid.poke(false.B)
    dut.io.i_data.bits.poke(0.U)
    dut.io.i_last.poke(false.B)
    dut.io.o_window.ready.poke(true.B)
    dut.clock.step()

    var inputIndex = 0
    var outputIndex = 0
    var cycle = 0
    val maxCycles = replay.length + size + 16

    while (outputIndex < size && cycle < maxCycles) {
      dut.io.o_window.ready.poke(true.B)

      if (dut.io.o_window.valid.peek().litToBoolean) {
        val bin = dut.io.o_window.bits.fftBin.peek().litValue.toInt
        assert(bin == outputIndex, s"fftBin mismatch: expected $outputIndex got $bin")
        assert(dut.io.o_window.bits.cut.peek().litValue == BigInt(outputIndex))
        assert(dut.io.o_window.bits.prev.peek().litValue == BigInt(wrap(outputIndex - 1, size)))
        assert(dut.io.o_window.bits.next.peek().litValue == BigInt(wrap(outputIndex + 1, size)))
        assert(dut.io.o_window.bits.last.peek().litToBoolean == (outputIndex == size - 1))

        for (index <- 0 until referenceCells) {
          val expectedLeft = wrap(outputIndex - guardCells - referenceCells + index, size)
          val expectedRight = wrap(outputIndex + guardCells + 1 + index, size)
          assert(dut.io.o_window.bits.leftRefs(index).peek().litValue == BigInt(expectedLeft))
          assert(dut.io.o_window.bits.rightRefs(index).peek().litValue == BigInt(expectedRight))
        }

        outputIndex += 1
      }

      if (inputIndex < replay.length) {
        assert(dut.io.i_data.ready.peek().litToBoolean, s"provider stalled with output ready at replay sample $inputIndex")
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(replay(inputIndex).U)
        dut.io.i_last.poke((inputIndex == replay.length - 1).B)
        inputIndex += 1
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }

      cycle += 1
      dut.clock.step()
    }

    assert(inputIndex == replay.length, s"fed $inputIndex of ${replay.length} replay samples")
    assert(outputIndex == size, s"observed $outputIndex of $size output windows")
  }

  private def replayAddrs(size: Int, referenceCells: Int, guardCells: Int): Seq[Int] = {
    val span = referenceCells + guardCells
    require(size > 2 * referenceCells + 2 * guardCells + 1)
    (0 until size + 2 * span).map(sample => wrap(sample - span, size))
  }

  private def wrap(index: Int, size: Int): Int = {
    val mod = index % size
    if (mod < 0) mod + size else mod
  }
}
