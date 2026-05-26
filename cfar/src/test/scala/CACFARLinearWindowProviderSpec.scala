package opera.cfar

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CACFARLinearWindowProviderSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "CACFARLinearWindowProvider"

  private val size           = 16
  private val referenceCells = 2
  private val guardCells     = 1
  private val params         = CACFARLinearWindowProviderTester.paramsForWindow(size, maxReferenceCells = 8, maxGuardCells = 2)

  it should "emit first, middle, and last linear windows with side sums and neighbors" in {
    test(new CACFARLinearWindowProvider(params)).withAnnotations(annotations) { dut =>
      CACFARLinearWindowProviderTester.driveFrame(dut, size, referenceCells, guardCells) { (outputIndex, activeDut) =>
        CACFARLinearWindowProviderTester.checkSemanticWindow(activeDut, outputIndex, size, referenceCells, guardCells)
        outputIndex match {
          case 0 =>
            activeDut.io.o_window.bits.rightSum.expect(CACFARLinearWindowProviderTester.sum(2, 3).U)
          case 5 =>
            activeDut.io.o_window.bits.leftSum.expect(CACFARLinearWindowProviderTester.sum(2, 3).U)
            activeDut.io.o_window.bits.rightSum.expect(CACFARLinearWindowProviderTester.sum(7, 8).U)
          case 15 =>
            activeDut.io.o_window.bits.leftSum.expect(CACFARLinearWindowProviderTester.sum(12, 13).U)
          case _ =>
        }
      }
    }
  }

  it should "hold the current payload stable under output backpressure" in {
    test(new CACFARLinearWindowProvider(params)).withAnnotations(annotations) { dut =>
      CACFARLinearWindowProviderTester.driveFrame(
        dut,
        size,
        referenceCells,
        guardCells,
        readyPattern = Seq(true, true, false, true, true, false, true)
      ) { (outputIndex, activeDut) =>
        CACFARLinearWindowProviderTester.checkSemanticWindow(activeDut, outputIndex, size, referenceCells, guardCells)
      }
    }
  }

  it should "keep large-window sums aligned through the CFAR backpressure pattern" in {
    val frameSize    = 32
    val refs         = 8
    val guards       = 2
    val span         = refs + guards
    val largeParams  = CACFARLinearWindowProviderTester.paramsForWindow(frameSize, maxReferenceCells = 16, maxGuardCells = 4)
    val readyPattern = Seq.fill(12)(true) ++ Seq.fill(4)(false) ++ Seq(true, false, false, true, true)

    test(new CACFARLinearWindowProvider(largeParams)).withAnnotations(annotations) { dut =>
      CACFARLinearWindowProviderTester.driveFrame(
        dut,
        frameSize,
        refs,
        guards,
        readyPattern = readyPattern
      ) { (outputIndex, activeDut) =>
        activeDut.io.o_window.bits.cut.expect(outputIndex.U)
        if (outputIndex >= span) {
          val leftRefs = (outputIndex - guards - refs) until (outputIndex - guards)
          val actualLeft = activeDut.io.o_window.bits.leftSum.peek().litValue
          val expectedLeft = BigInt(leftRefs.sum)
          assert(actualLeft == expectedLeft, s"leftSum mismatch at bin $outputIndex: expected $expectedLeft got $actualLeft")
        }
        if (outputIndex < frameSize - span) {
          val rightRefs = (outputIndex + guards + 1) to (outputIndex + guards + refs)
          activeDut.io.o_window.bits.rightSum.expect(BigInt(rightRefs.sum).U)
        }
      }
    }
  }

  it should "preserve full reference sums for max-value input frames" in {
    val frameSize = 32
    val refs      = 8
    val guards    = 2
    val span      = refs + guards
    val maxSample = 255
    val maxSum    = BigInt(refs * maxSample)
    val maxParams = CFARParams(
      inputType         = UInt(8.W),
      thresholdType     = UInt(8.W),
      scaleType         = UInt(8.W),
      maxFftSize        = frameSize,
      maxReferenceCells = refs,
      maxGuardCells     = guards,
      edgePolicy        = CFAREdgePolicy.OneSidedAverage
    )

    test(new CACFARLinearWindowProvider(maxParams)).withAnnotations(annotations) { dut =>
      CACFARLinearWindowProviderTester.driveFrame(
        dut,
        frameSize,
        refs,
        guards,
        inputValue = _ => maxSample
      ) { (outputIndex, activeDut) =>
        if (outputIndex >= span) {
          activeDut.io.o_window.bits.leftSum.expect(maxSum.U)
        }
        if (outputIndex < frameSize - span) {
          activeDut.io.o_window.bits.rightSum.expect(maxSum.U)
        }
      }
    }
  }

  it should "assert when i_last arrives before the configured frame end" in {
    intercept[Throwable] {
      test(new CACFARLinearWindowProvider(params)).withAnnotations(annotations) { dut =>
        CACFARLinearWindowProviderTester.driveLastAlignmentViolation(
          dut,
          size,
          referenceCells,
          guardCells,
          lastIndex = size - 2,
          maxInputs = size
        )
      }
    }
  }

  it should "assert when i_last arrives after the configured frame end" in {
    intercept[Throwable] {
      test(new CACFARLinearWindowProvider(params)).withAnnotations(annotations) { dut =>
        CACFARLinearWindowProviderTester.driveLastAlignmentViolation(
          dut,
          size,
          referenceCells,
          guardCells,
          lastIndex = size,
          maxInputs = size + 1
        )
      }
    }
  }
}
