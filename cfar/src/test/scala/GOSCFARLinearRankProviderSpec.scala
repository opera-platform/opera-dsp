package opera.cfar

import chisel3._
import chiseltest._
import opera.lis.LISType
import org.scalatest.flatspec.AnyFlatSpec

class GOSCFARLinearRankProviderSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "GOSCFARLinearRankProvider"

  private def checkRankWindowFrame(
    params      : CFARParams[UInt],
    frame       : Seq[Int],
    refs        : Int,
    guards      : Int,
    leftRank    : Int,
    rightRank   : Int,
    readyPattern: Seq[Boolean] = Seq(true)
  )(check: (Int, GOSCFARLinearRankProvider[UInt]) => Unit): Unit = {
    test(new GOSCFARLinearRankProvider(params)).withAnnotations(annotations) { dut =>
      GOSCFARLinearRankProviderTester.driveFrame(dut, frame, refs, guards, leftRank, rightRank, readyPattern)(check)
    }
  }

  private def expectLastAlignmentAssertion(lastIndex: Int, maxInputs: Int): Unit = {
    val frameSize = 16
    intercept[Throwable] {
      test(new GOSCFARLinearRankProvider(GOSCFARLinearRankProviderTester.providerParams(frameSize, 2, 1))).withAnnotations(annotations) { dut =>
        GOSCFARLinearRankProviderTester.driveLastAlignmentViolation(dut, frameSize, lastIndex, maxInputs)
      }
    }
  }

  for (lisType <- LISType.all) {
    it should s"emit ranked linear windows with edge flags and neighbors using $lisType" in {
      val frame  = (0 until 16)
      val refs   = 2
      val guards = 1
      checkRankWindowFrame(
        GOSCFARLinearRankProviderTester.providerParams(frame.length, refs, guards, lisType),
        frame,
        refs,
        guards,
        leftRank  = 1,
        rightRank = 2
      ) { (bin, dut) =>
        GOSCFARLinearRankProviderTester.checkWindow(dut, frame, refs, guards, leftRank = 1, rightRank = 2, bin)
      }
    }
  }

  it should "rank unsorted duplicate reference values deterministically" in {
    val frame  = Seq(7, 3, 7, 2, 9, 2, 5, 5, 8, 1, 8, 4, 6, 4, 6, 0)
    val refs   = 2
    val guards = 1
    checkRankWindowFrame(GOSCFARLinearRankProviderTester.providerParams(frame.length, refs, guards), frame, refs, guards, leftRank = 2, rightRank = 1) {
      (bin, dut) => GOSCFARLinearRankProviderTester.checkWindow(dut, frame, refs, guards, leftRank = 2, rightRank = 1, bin)
    }
  }

  it should "emit ranked windows with one reference cell per side" in {
    val frame = Seq(9, 1, 5, 2, 7, 3, 6, 4)
    checkRankWindowFrame(GOSCFARLinearRankProviderTester.providerParams(frame.length, 1, 1), frame, refs = 1, guards = 1, leftRank = 1, rightRank = 1) {
      (bin, dut) => GOSCFARLinearRankProviderTester.checkWindow(dut, frame, refs = 1, guards = 1, leftRank = 1, rightRank = 1, bin)
    }
  }

  it should "hold ranked window payload stable while output is backpressured" in {
    val frame  = Seq(7, 3, 7, 2, 9, 2, 5, 5, 8, 1, 8, 4, 6, 4, 6, 0)
    val refs   = 2
    val guards = 1
    checkRankWindowFrame(
      GOSCFARLinearRankProviderTester.providerParams(frame.length, refs, guards),
      frame,
      refs,
      guards,
      leftRank     = 1,
      rightRank    = 2,
      readyPattern = Seq(true, true, false, false, true, true, false, true)
    ) { (bin, dut) =>
      GOSCFARLinearRankProviderTester.checkWindow(dut, frame, refs, guards, leftRank = 1, rightRank = 2, bin)
    }
  }

  it should "reject i_last before the configured frame end" in {
    expectLastAlignmentAssertion(lastIndex = 14, maxInputs = 16)
  }

  it should "reject i_last after the configured frame end" in {
    expectLastAlignmentAssertion(lastIndex = 16, maxInputs = 17)
  }
}
