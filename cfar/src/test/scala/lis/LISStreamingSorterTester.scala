package opera.lis

import chisel3._
import chiseltest._
import dsptools.numbers.Real
import fixedpoint._
import opera.cfar.TestConfig

import scala.util.Random

object LISStreamingSorterTester {
  private final case class OutputSnapshot(bits: BigInt, last: Boolean)

  def literalFor[T <: Data](value: Double, dataType: T): T = dataType match {
    case u: UInt =>
      BigInt(math.round(value)).U(u.getWidth.W).asInstanceOf[T]
    case s: SInt =>
      BigInt(math.round(value)).S(s.getWidth.W).asInstanceOf[T]
    case f: FixedPoint =>
      require(f.binaryPoint.known, "FixedPoint tests require a known binary point")
      val binaryPoint = f.binaryPoint.get
      val raw = BigInt(math.round(value * math.pow(2.0, binaryPoint.toDouble)))
      FixedPoint.fromBigInt(raw, f.getWidth.W, binaryPoint.BP).asInstanceOf[T]
    case other =>
      throw new IllegalArgumentException(s"Unsupported LIS test literal type: ${other.getClass.getName}")
  }

  private def outputSnapshot[T <: Data: Real](dut: LIS[T]): OutputSnapshot =
    OutputSnapshot(
      bits = dut.io.o_data.bits match {
        case value: FixedPoint => value.asSInt.peek().litValue
        case value: UInt       => value.peek().litValue
        case value: SInt       => value.peek().litValue
        case other             => throw new IllegalArgumentException(s"Unsupported LIS debug signal type: ${other.getClass.getName}")
      },
      last = dut.io.o_last.peek().litToBoolean
    )

  def initialize[T <: Data: Real](
    dut       : LIS[T],
    activeSize: Option[Int] = None
  ): Unit = {
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_data.bits.poke(literalFor(0.0, dut.params.dataType))
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(true.B)
    dut.io.i_window_size.foreach(_.poke(activeSize.getOrElse(dut.params.maxWindowSize).U))
    dut.clock.step()
    dut.io.o_data.valid.expect(false.B)
  }

  def drive[T <: Data: Real](
    dut                   : LIS[T],
    values                : Seq[Double],
    assertLastOnFinalInput: Boolean = false,
    randomReadyValid      : Boolean = TestConfig.randomReadyValid,
    seed                  : Long = 0xC0FFEEL,
    model                 : Option[LISStreamingModel] = None
  ): Unit = {
    val dataType     = dut.params.dataType
    val rng          = new Random(seed)
    val maxCycles    = if (randomReadyValid) values.length * 40 + 40 else values.length + 4
    var index        = 0
    var cycle        = 0
    var inputStarted = false
    var heldOutput: Option[OutputSnapshot] = None
    // Per-beat model expectation produced on the accepting cycle and checked after the following clock step, once the sorter registers have updated.
    var pendingBeat: Option[LISStreamingModel.Beat] = None
    var enteringFlush = false

    while (index < values.length && cycle < maxCycles) {
      if (!inputStarted) {
        inputStarted = !randomReadyValid || rng.nextDouble() < 0.8
      }
      val driveInput  = inputStarted
      val outputReady = !randomReadyValid || rng.nextDouble() < 0.8
      val value       = values(index)

      dut.io.o_data.ready.poke(outputReady.B)
      dut.io.i_data.valid.poke(driveInput.B)
      dut.io.i_data.bits.poke(literalFor(value, dut.params.dataType))
      dut.io.i_last.poke((driveInput && assertLastOnFinalInput && index == values.length - 1).B)

      val inReady  = dut.io.i_data.ready.peek().litToBoolean
      val outValid = dut.io.o_data.valid.peek().litToBoolean
      if (outValid) {
        val current = outputSnapshot(dut)
        heldOutput.foreach { held =>
          assert(current == held, s"LIS output changed while backpressured: previous=$held current=$current")
        }

        if (outputReady) {
          heldOutput = None
        } else if (heldOutput.isEmpty) {
          heldOutput = Some(current)
        }
      } else {
        if (heldOutput.nonEmpty) {
          assert(false, s"LIS o_data.valid dropped while output was backpressured: held=${heldOutput.get}")
        }
      }

      if (TestConfig.verbose) {
        println(
          f"drive cycle=$cycle%4d inputIndex=$index%3d valid=$driveInput ready=$inReady " +
            s"outValid=$outValid outReady=$outputReady value=$value"
        )
      }

      if (driveInput && inReady) {
        model.foreach { m =>
          val isLastInput = index == values.length - 1
          val beat        = m.accept(value)
          // o_data.bits carries the removed oldest FIFO sample combinationally on the accepting cycle; it is meaningful only once the window is full.
          beat.removedFifo.foreach { removed =>
            dut.io.o_data.valid.expect(true.B)
            dut.io.o_data.bits.expect(literalFor(removed, dataType))
          }
          dut.io.o_last.expect(false.B)
          pendingBeat   = Some(beat)
          enteringFlush = assertLastOnFinalInput && isLastInput
        }
        index += 1
        inputStarted = false
      }
      dut.clock.step()

      pendingBeat.foreach { beat =>
        // After i_last the frame enters flush, where o_sorter_full drops and the sorted vector is architecture-dependent, so it is left unchecked here.
        if (!enteringFlush) {
          dut.io.o_sorter_full.expect(beat.full.B)
          if (beat.full) {
            beat.sortedAfter.zipWithIndex.foreach { case (value, sortedIndex) =>
              dut.io.o_sorted_data(sortedIndex).expect(literalFor(value, dataType))
            }
          }
        }
        pendingBeat = None
      }
      cycle += 1
    }

    assert(index == values.length, s"LIS drive accepted $index of ${values.length} inputs after $cycle cycles")
    dut.io.i_data.valid.poke(false.B)
    dut.io.i_last.poke(false.B)
    dut.io.o_data.ready.poke(true.B)
  }

  /**
   * Drives one full frame (fill, process, then an `i_last` flush) and checks every accepted beat against a fresh [[LISStreamingModel]].
   */
  def runStream[T <: Data: Real](
    dut             : LIS[T],
    values          : Seq[Double],
    activeSize      : Int,
    seed            : Long,
    randomReadyValid: Boolean = TestConfig.randomReadyValid
  ): Unit = {
    val model = new LISStreamingModel(dut.params.maxWindowSize, activeSize)
    initialize(dut, activeSize = Some(activeSize))
    drive(
      dut,
      values,
      assertLastOnFinalInput = true,
      randomReadyValid = randomReadyValid,
      seed = seed,
      model = Some(model)
    )
    expectFlush(dut, expectedOutputs = model.flushLength, randomReadyValid = randomReadyValid, seed = seed + 1)
  }

  def expectSorted[T <: Data: Real](
    dut       : LIS[T],
    values    : Seq[Double],
    activeSize: Option[Int] = None
  ): Unit = {
    val count    = activeSize.getOrElse(values.length)
    val expected = values.takeRight(count).sorted

    expected.zipWithIndex.foreach { case (value, index) =>
      dut.io.o_sorted_data(index).expect(literalFor(value, dut.params.dataType))
    }

    if (TestConfig.verbose) {
      println(s"sorted expected=${expected.mkString("[", ", ", "]")} activeSize=$count")
    }
  }

  def expectFlush[T <: Data: Real](
    dut             : LIS[T],
    expectedOutputs : Int,
    randomReadyValid: Boolean = TestConfig.randomReadyValid,
    seed            : Long = 0xC0FFEEL
  ): Unit = {
    val rng         = new Random(seed)
    val maxCycles   = if (randomReadyValid) expectedOutputs * 40 + 40 else expectedOutputs + 4
    var outputIndex = 0
    var cycle       = 0
    var heldOutput: Option[OutputSnapshot] = None

    while (outputIndex < expectedOutputs && cycle < maxCycles) {
      val outputReady = !randomReadyValid || rng.nextDouble() < 0.8
      dut.io.i_data.valid.poke(false.B)
      dut.io.i_last.poke(false.B)
      dut.io.o_data.ready.poke(outputReady.B)

      val outValid = dut.io.o_data.valid.peek().litToBoolean
      if (outValid) {
        val current = outputSnapshot(dut)
        heldOutput.foreach { held =>
          assert(current == held, s"LIS flush output changed while backpressured: previous=$held current=$current")
        }

        if (outputReady) {
          dut.io.o_last.expect((outputIndex == expectedOutputs - 1).B)
          outputIndex += 1
          heldOutput = None
        } else if (heldOutput.isEmpty) {
          heldOutput = Some(current)
        }
      } else {
        if (heldOutput.nonEmpty) {
          assert(false, s"LIS flush o_data.valid dropped while output was backpressured: held=${heldOutput.get}")
        }
      }

      if (TestConfig.verbose) {
        println(
          f"flush cycle=$cycle%4d outputIndex=$outputIndex%3d " +
            s"outValid=$outValid outReady=$outputReady"
        )
      }

      dut.clock.step()
      cycle += 1
    }

    assert(outputIndex == expectedOutputs, s"LIS flush produced $outputIndex of $expectedOutputs outputs")
    dut.io.o_data.ready.poke(true.B)
    dut.io.o_data.valid.expect(false.B)
  }
}
