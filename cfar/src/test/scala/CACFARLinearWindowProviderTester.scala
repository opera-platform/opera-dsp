package opera.cfar

import chisel3._
import chiseltest._

private[cfar] object CACFARLinearWindowProviderTester {
  def paramsForWindow(frameSize: Int, maxReferenceCells: Int, maxGuardCells: Int): CFARParams[UInt] =
    CFARParams(
      inputType         = UInt(16.W),
      thresholdType     = UInt(16.W),
      scaleType         = UInt(16.W),
      maxFftSize        = frameSize,
      maxReferenceCells = maxReferenceCells,
      maxGuardCells     = maxGuardCells,
      edgePolicy       = CFAREdgePolicy.OneSidedAverage
    )

  def sum(values: Int*): BigInt = BigInt(values.sum)

  def checkSemanticWindow(
      dut           : CACFARLinearWindowProvider[UInt],
      outputIndex   : Int,
      frameSize     : Int,
      referenceCells: Int,
      guardCells    : Int
  ): Unit = {
    val span = referenceCells + guardCells
    dut.io.o_window.bits.cut.expect(outputIndex.U)
    dut.io.o_window.bits.isLeftEdge.expect((outputIndex < span).B)
    dut.io.o_window.bits.isRightEdge.expect((outputIndex >= frameSize - span).B)
    if (outputIndex > 0) {
      dut.io.o_window.bits.prev.expect((outputIndex - 1).U)
    }
    if (outputIndex < frameSize - 1) {
      dut.io.o_window.bits.next.expect((outputIndex + 1).U)
    }
    if (outputIndex >= span) {
      val leftRefs = (outputIndex - guardCells - referenceCells) until (outputIndex - guardCells)
      dut.io.o_window.bits.leftSum.expect(BigInt(leftRefs.sum).U)
    }
    if (outputIndex < frameSize - span) {
      val rightRefs = (outputIndex + guardCells + 1) to (outputIndex + guardCells + referenceCells)
      dut.io.o_window.bits.rightSum.expect(BigInt(rightRefs.sum).U)
    }
  }

  def driveFrame(
      dut: CACFARLinearWindowProvider[UInt],
      frameSize     : Int,
      referenceCells: Int,
      guardCells    : Int,
      inputValue    : Int => Int = identity,
      readyPattern  : Seq[Boolean] = Seq(true)
  )(checkOutput: (Int, CACFARLinearWindowProvider[UInt]) => Unit): Unit = {
    require(readyPattern.nonEmpty, "readyPattern must not be empty")

    dut.io.i_fft_size.poke(frameSize.U)
    dut.io.i_reference_cells.poke(referenceCells.U)
    dut.io.i_guard_cells.poke(guardCells.U)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_data.bits.poke(0.U)
    dut.io.i_last.poke(false.B)
    dut.io.i_output_done.poke(false.B)
    dut.io.o_window.ready.poke(readyPattern.head.B)
    dut.clock.step()

    var inputIndex = 0
    var outputIndex = 0
    var cycle = 0
    val maxCycles = frameSize * 4
    var heldPayload: Option[(BigInt, BigInt, BigInt, BigInt, Boolean)] = None

    while (outputIndex < frameSize && cycle < maxCycles) {
      val outputReady = readyPattern.lift(cycle).getOrElse(readyPattern.last)
      dut.io.o_window.ready.poke(outputReady.B)
      dut.io.i_output_done.poke(false.B)

      if (dut.io.o_window.valid.peek().litToBoolean) {
        val payload = (
          dut.io.o_window.bits.fftBin.peek().litValue,
          dut.io.o_window.bits.cut.peek().litValue,
          dut.io.o_window.bits.leftSum.peek().litValue,
          dut.io.o_window.bits.rightSum.peek().litValue,
          dut.io.o_window.bits.last.peek().litToBoolean
        )
        if (outputReady) {
          heldPayload.foreach(held => assert(payload == held, "provider output changed before the held payload fired"))
          heldPayload = None
          checkOutput(outputIndex, dut)
          dut.io.o_window.bits.fftBin.expect(outputIndex.U)
          dut.io.o_window.bits.last.expect((outputIndex == frameSize - 1).B)
          if (outputIndex == frameSize - 1) {
            dut.io.i_output_done.poke(true.B)
          }
          outputIndex += 1
        } else {
          heldPayload.foreach(held => assert(payload == held, "provider output changed while backpressured"))
          if (heldPayload.isEmpty) {
            heldPayload = Some(payload)
          }
        }
      }

      if (inputIndex < frameSize && dut.io.i_data.ready.peek().litToBoolean) {
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(inputValue(inputIndex).U)
        dut.io.i_last.poke((inputIndex == frameSize - 1).B)
        inputIndex += 1
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }

      cycle += 1
      dut.clock.step()
    }

    assert(outputIndex == frameSize, s"observed $outputIndex of $frameSize windows")
  }

  def driveLastAlignmentViolation(
      dut           : CACFARLinearWindowProvider[UInt],
      fftSize       : Int,
      referenceCells: Int,
      guardCells    : Int,
      lastIndex     : Int,
      maxInputs     : Int
  ): Unit = {
    dut.io.i_fft_size.poke(fftSize.U)
    dut.io.i_reference_cells.poke(referenceCells.U)
    dut.io.i_guard_cells.poke(guardCells.U)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_data.bits.poke(0.U)
    dut.io.i_last.poke(false.B)
    dut.io.i_output_done.poke(false.B)
    dut.io.o_window.ready.poke(true.B)
    dut.clock.step()

    var inputIndex = 0
    var cycle = 0
    val maxCycles = fftSize * 5

    while (cycle < maxCycles) {
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
}
