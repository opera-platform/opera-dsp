package opera.cfar

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CFARDelayCellsSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CFAR delay-cell helpers"

  /**
   * Checks the simple register delay implementation at a fixed runtime depth. The stream must preserve sample order and mark `o_last` on the final output.
   */
  it should "delay a stream through a register-backed delay line" in {
    test(new DelayRegisterCells(UInt(8.W), maxDepth = 4))
      .withAnnotations(TestConfig.annotations) { dut =>
        for (depth <- Seq(1, 2, 3, 4)) {
          val input = Seq(1, 2, 3, 4, 5, 6).map(_ + 10 * depth)
          CFARDelayCellsTester.drainRegisterDelay(dut, input, depth)
        }
      }
  }

  /**
   * Checks the explicit depth-zero path where register delay should behave as a pure Decoupled pass-through and never report a full delayed window.
   */
  it should "pass a register-backed delay through at zero depth" in {
    test(new DelayRegisterCells(UInt(8.W), maxDepth = 4))
      .withAnnotations(TestConfig.annotations) { dut =>
        CFARDelayCellsTester.drainRegisterDelay(
          dut,
          Seq(7, 8, 9, 10),
          depth = 0,
          readyPattern = Seq(true, false, true, true),
          expectFull = false
        )
      }
  }

  /**
   * Exercises the `maxDepth == 1` tap-selection fast path with normal delayed streaming behavior and final-sample flush.
   */
  it should "support a one-deep register-backed delay line" in {
    test(new DelayRegisterCells(UInt(8.W), maxDepth = 1))
      .withAnnotations(TestConfig.annotations) { dut =>
        CFARDelayCellsTester.drainRegisterDelay(
          dut,
          Seq(31, 32, 33, 34),
          depth = 1,
          readyPattern = Seq(true, true, false, true, true, true)
        )
      }
  }

  /**
   * Exercises reference delays that stay below the SRAM threshold so the register-backed path reports full/empty status and delayed data correctly.
   */
  it should "use the register reference delay below the SRAM threshold" in {
    test(new ReferenceDelayCells(UInt(8.W), maxDepth = 4, minSRAMDepth = 4))
      .withAnnotations(TestConfig.annotations) { dut =>
        for (depth <- Seq(1, 2, 3, 4)) {
          CFARDelayCellsTester.drainReferenceDelay(dut, Seq(1, 2, 3, 4, 5, 6, 7), depth)
        }
      }
  }

  /**
   * Exercises reference delays at and above the SRAM threshold so the memory backed path matches the register-delay ordering contract.
   */
  it should "use the SRAM reference delay above the SRAM threshold" in {
    test(new ReferenceDelayCells(UInt(8.W), maxDepth = 8, minSRAMDepth = 4))
      .withAnnotations(TestConfig.annotations) { dut =>
        for (depth <- Seq(4, 5, 7, 8)) {
          CFARDelayCellsTester.drainReferenceDelay(dut, Seq(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), depth)
        }
      }
  }

  /**
   * Checks the explicit depth zero path through the SRAM-backed selector.  Data should bypass the memory and preserve ready/valid backpressure.
   */
  it should "pass an SRAM-backed reference delay through at zero depth" in {
    test(new ReferenceDelayCells(UInt(8.W), maxDepth = 8, minSRAMDepth = 4))
      .withAnnotations(TestConfig.annotations) { dut =>
        CFARDelayCellsTester.drainReferenceDelay(
          dut,
          Seq(41, 42, 43, 44),
          depth = 0,
          readyPattern = Seq(true, false, true, true),
          expectFull = false
        )
      }
  }

  /**
   * Stalls the reference-delay output while data is flowing to prove ready/valid backpressure does not drop, duplicate, or reorder samples.
   */
  it should "preserve reference-delay data under output backpressure" in {
    test(new ReferenceDelayCells(UInt(8.W), maxDepth = 8, minSRAMDepth = 4))
      .withAnnotations(TestConfig.annotations) { dut =>
        CFARDelayCellsTester.drainReferenceDelay(
          dut,
          Seq(21, 22, 23, 24, 25, 26, 27, 28),
          depth = 4,
          readyPattern = Seq(true, true, true, false, true, false, true)
        )
      }
  }
}
