package opera.cfar

import chisel3._
import chiseltest._

private[cfar] object CFARDelayCellsTester {
  /**
   * Drives a register delay and checks delayed samples plus optional full-state observation.  Depth zero is a pure ready/valid pass-through.
   */
  def drainRegisterDelay(
    dut         : DelayRegisterCells[UInt],
    input       : Seq[Int],
    depth       : Int,
    readyPattern: Seq[Boolean] = Seq(true),
    expectFull  : Boolean = true
  ): Unit = {
    require(readyPattern.nonEmpty, "readyPattern must not be empty")

    dut.io.i_depth.poke(depth.U)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(readyPattern.head.B)
    dut.io.o_empty.expect(true.B)
    dut.clock.step()

    var inputIndex  = 0
    var outputIndex = 0
    var cycle       = 0
    var sawFull     = false
    while (outputIndex < input.length && cycle < 4000) {
      if (inputIndex < input.length) {
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(input(inputIndex).U)
        dut.io.i_last.poke((inputIndex == input.length - 1).B)
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }
      dut.io.o_data.ready.poke(readyPattern.lift(cycle).getOrElse(readyPattern.last).B)

      val inFire = dut.io.i_data.valid.peek().litToBoolean && dut.io.i_data.ready.peek().litToBoolean
      val outFire = dut.io.o_data.valid.peek().litToBoolean && dut.io.o_data.ready.peek().litToBoolean
      sawFull ||= dut.io.o_full.peek().litToBoolean
      if (outFire) {
        dut.io.o_data.bits.expect(input(outputIndex).U)
        if (outputIndex == input.length - 1) dut.io.o_last.expect(true.B)
        outputIndex += 1
      }
      if (inFire) inputIndex += 1
      cycle += 1
      dut.clock.step()
    }

    assert(outputIndex == input.length, s"Only observed $outputIndex of ${input.length} output samples")
    if (expectFull) {
      assert(sawFull, "Register delay never reported a full window")
    } else {
      assert(!sawFull, "Register delay reported full while in pass-through mode")
    }
    dut.io.o_full.expect(false.B)
    dut.io.o_empty.expect(true.B)
  }

  /**
   * Drives a reference-delay helper and checks delayed samples, full/empty state, and optional output backpressure handling.
   */
  def drainReferenceDelay(
    dut: ReferenceDelayCells[UInt],
    input: Seq[Int],
    depth: Int,
    readyPattern: Seq[Boolean] = Seq(true),
    expectFull: Boolean = true
  ): Unit = {
    require(readyPattern.nonEmpty, "readyPattern must not be empty")

    dut.io.i_depth.poke(depth.U)
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(readyPattern.head.B)
    dut.io.o_empty.expect(true.B)
    dut.clock.step()

    var inputIndex = 0
    var outputIndex = 0
    var cycle = 0
    var sawFull = false
    while (outputIndex < input.length && cycle < 4000) {
      if (inputIndex < input.length) {
        dut.io.i_data.valid.poke(true.B)
        dut.io.i_data.bits.poke(input(inputIndex).U)
        dut.io.i_last.poke((inputIndex == input.length - 1).B)
      } else {
        dut.io.i_data.valid.poke(false.B)
        dut.io.i_last.poke(false.B)
      }
      dut.io.o_data.ready.poke(readyPattern.lift(cycle).getOrElse(readyPattern.last).B)

      val inFire = dut.io.i_data.valid.peek().litToBoolean && dut.io.i_data.ready.peek().litToBoolean
      val outFire = dut.io.o_data.valid.peek().litToBoolean && dut.io.o_data.ready.peek().litToBoolean
      sawFull ||= dut.io.o_full.peek().litToBoolean
      if (outFire) {
        dut.io.o_data.bits.expect(input(outputIndex).U)
        if (outputIndex == input.length - 1) dut.io.o_last.expect(true.B)
        outputIndex += 1
      }
      if (inFire) inputIndex += 1
      cycle += 1
      dut.clock.step()
    }

    assert(outputIndex == input.length, s"Only observed $outputIndex of ${input.length} output samples")
    if (expectFull) {
      assert(sawFull, "Reference delay never reported a full window")
    } else {
      assert(!sawFull, "Reference delay reported full while in pass-through mode")
    }
    dut.io.o_full.expect(false.B)
    dut.io.o_empty.expect(true.B)
  }
}
