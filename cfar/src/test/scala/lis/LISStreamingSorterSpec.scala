package opera.lis

import chisel3._
import chiseltest._
import dsptools.numbers._
import fixedpoint._
import opera.cfar.{TestConfig, TestConfigSupport}
import org.scalatest.flatspec.AnyFlatSpec

class LISStreamingSorterSpec extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of "LIS linear streaming insertion sorters"

  private val annotations = TestConfig.annotations

  private def caseName(pairs: (String, Any)*): String = pairs.map { case (key, value) => s"$key=$value" }.mkString(", ")

  private val windowSizes = Seq(2, 3, 4, 8, 16)
  private val seeds       = Seq(0xA11CEL, 0xB0BL, 0xC0FFEEL)

  for {
    sorterType <- LISType.all
    runTime <- Seq(false, true)
  } {
    it should s"stream one-lane windows with ${caseName("sorter" -> sorterType, "runTime" -> runTime)}" in {
      val params = LISParams[UInt](UInt(8.W), maxWindowSize = 1, sorterType = sorterType, runTime = runTime)

      test(new LIS(params)).withAnnotations(annotations) { dut =>
        LISStreamingSorterTester.runStream(
          dut,
          values = Seq(12.0, 7.0, 19.0, 3.0),
          activeSize = 1,
          seed = 0x1A11L,
          randomReadyValid = true
        )
      }
    }
  }

  /*  
  * Randomized config matrix: every architecture, type, window size, static and runtime, across several seeds. 
  * Each case drives a random frame and checks o_sorted_data, o_data.bits, o_last and o_sorter_full per beat against the streaming model, 
  * then flushes the frame.
  */
  private def streamingMatrix[T <: Data: Real](typeName: String, dataType: T): Unit =
    for {
      sorterType <- LISType.all
      maxWindow  <- windowSizes
      runTime    <- Seq(false, true)
      seed       <- seeds
      activeSize <- if (runTime) Seq(1, (maxWindow + 1) / 2, maxWindow).distinct else Seq(maxWindow)
    } {
      it should s"match the streaming model with ${caseName(
          "sorter"  -> sorterType,
          "type"    -> typeName,
          "max"     -> maxWindow,
          "active"  -> activeSize,
          "runTime" -> runTime,
          "seed"    -> seed.toHexString
        )}" in {
        val params = LISParams[T](dataType, maxWindow, sorterType, runTime)
        val stream = LISStreamScenarios.randomStream(seed, length = math.ceil(maxWindow * 2.5).toInt, dataType)
        test(new LIS(params)).withAnnotations(annotations) { dut =>
          LISStreamingSorterTester.runStream(dut, stream, activeSize = activeSize, seed = seed)
        }
      }
    }

  streamingMatrix("UInt", UInt(8.W))
  streamingMatrix("SInt", SInt(8.W))
  streamingMatrix("FixedPoint", FixedPoint(12.W, 4.BP))

  /* 
  * Named edge cases that random streams do not reliably exercise.
  */

  // Numeric extremes and duplicates of the maximum value, per type.
  private val extremeStreams: Seq[(String, Data, Seq[Double])] = Seq(
    ("UInt", UInt(8.W), Seq(255.0, 0.0, 128.0, 255.0, 1.0)),
    ("SInt", SInt(8.W), Seq(-128.0, 127.0, 0.0, -1.0, 126.0)),
    ("FixedPoint", FixedPoint(12.W, 4.BP), Seq(-128.0, 127.9375, 0.5, -0.5, 127.9375))
  )

  for {
    sorterType <- LISType.all
    (typeName, dataType, stream) <- extremeStreams
  } {
    it should s"sort numeric min and max values with ${caseName("sorter" -> sorterType, "type" -> typeName)}" in {
      dataType match {
        case u: UInt =>
          test(new LIS(LISParams[UInt](u, 4, sorterType))).withAnnotations(annotations) { dut =>
            LISStreamingSorterTester.runStream(dut, stream, activeSize = 4, seed = 0x5A11L)
          }
        case s: SInt =>
          test(new LIS(LISParams[SInt](s, 4, sorterType))).withAnnotations(annotations) { dut =>
            LISStreamingSorterTester.runStream(dut, stream, activeSize = 4, seed = 0x5A12L)
          }
        case f: FixedPoint =>
          test(new LIS(LISParams[FixedPoint](f, 4, sorterType))).withAnnotations(annotations) { dut =>
            LISStreamingSorterTester.runStream(dut, stream, activeSize = 4, seed = 0x5A13L)
          }
      }
    }
  }

  // The oldest of two equal values must be the one evicted.
  // The model and the per-beat o_data.bits check inside runStream verify that 5 (the first one) leaves while one duplicate stays active.
  for (sorterType <- LISType.all) {
    it should s"evict the duplicated oldest FIFO value with ${caseName("sorter" -> sorterType)}" in {
      test(new LIS(LISParams[UInt](UInt(8.W), 4, sorterType))).withAnnotations(annotations) { dut =>
        LISStreamingSorterTester.runStream(dut, Seq(5.0, 1.0, 5.0, 2.0, 7.0), activeSize = 4, seed = 0xD0BL)
      }
    }
  }

  // A mid-frame i_window_size poke must be ignored until the sorter returns to idle; 
  // The next frame then adopts the new size. Covers both "ignored until idle" and "two frames with different runtime sizes".
  for (sorterType <- LISType.all) {
    it should s"ignore runtime size changes until idle, then accept a new frame size with ${caseName("sorter" -> sorterType)}" in {
      val params = LISParams[UInt](UInt(8.W), maxWindowSize = 6, sorterType = sorterType, runTime = true)
      val firstFramePrefix  = Seq(9.0, 1.0, 7.0, 3.0)
      val firstFrameUpdates = Seq(6.0, 2.0)
      val firstFrameLast    = Seq(8.0)
      val secondFrame       = Seq(5.0, 4.0)

      test(new LIS(params)).withAnnotations(annotations) { dut =>
        LISStreamingSorterTester.initialize(dut, activeSize = Some(4))
        LISStreamingSorterTester.drive(dut, firstFramePrefix)

        dut.io.i_window_size.foreach(_.poke(2.U))
        LISStreamingSorterTester.drive(dut, firstFrameUpdates)
        LISStreamingSorterTester.expectSorted(dut, firstFramePrefix ++ firstFrameUpdates, activeSize = Some(4))

        LISStreamingSorterTester.drive(dut, firstFrameLast, assertLastOnFinalInput = true)
        LISStreamingSorterTester.expectFlush(dut, expectedOutputs = 4)

        dut.io.i_window_size.foreach(_.poke(2.U))
        dut.clock.step()
        LISStreamingSorterTester.drive(dut, secondFrame)
        LISStreamingSorterTester.expectSorted(dut, secondFrame, activeSize = Some(2))
      }
    }
  }
}
