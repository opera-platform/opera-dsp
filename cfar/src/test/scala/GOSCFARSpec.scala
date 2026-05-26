package opera.cfar

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import dsptools.numbers._
import fixedpoint._
import opera.lis.LISType
import org.scalatest.flatspec.AnyFlatSpec
import opera.cfar.CFARModel.expectedGOSFrame
import opera.cfar.GOSCFARTester.{
  configure,
  expectFrame,
  expectInputBackpressureWhenFrameBuffersFill,
  expectOutputStableWhileBackpressured
}

class GOSCFARSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "GOS-CFAR"

  private val architectures = LISType.all
  private val defaultWindow = CFARTestWindowCase(16, 4, 1)
  private val fixedType     = FixedPoint(16.W, 6.BP)
  private val asymmetricFrame = Seq(
    20.0,  1.0, 40.0,  10.0,
     7.0, 50.0,  5.0, 100.0,
     6.0, 70.0,  8.0,  30.0,
     2.0, 60.0,  3.0,  90.0
  )
  private val duplicateFrame = Seq(
     4.0,  4.0, 12.0, 12.0,
    32.0, 18.0, 32.0,  8.0,
     8.0, 20.0, 20.0,  6.0,
     6.0, 14.0, 14.0,  2.0
  )
  private val guardAndCutStressFrame = Seq(
      1.0,   2.0,   3.0,  4.0,
    250.0, 240.0, 250.0,  5.0,
      6.0,   7.0,   8.0,  9.0,
     10.0,  11.0,  12.0, 13.0
  )

  private def gosParamsFor[T <: Data: Real](
    dataType         : T,
    lisType          : String,
    window           : CFARTestWindowCase = defaultWindow,
    sendCut          : Boolean = true,
    logMode          : Boolean = false,
    runtimeLogMode   : Boolean = false,
    edgePolicy       : Int = CFAREdgePolicy.SuppressEdges,
    runtimeEdgePolicy: Boolean = false,
    retiming         : Boolean = false,
    addPipeStages    : Int = 0,
    mulPipeStages    : Int = 0,
    maxReferenceCells: Int = 4,
    maxGuardCells    : Int = 2
  ): CFARParams[T] = {
    paramsForWindow(
      dataType,
      window,
      maxReferenceCells = maxReferenceCells,
      maxGuardCells     = maxGuardCells,
      cfarType          = CFARType.OrderedStatistic,
      lisType           = lisType,
      sendCut           = sendCut,
      logMode           = logMode,
      runtimeLogMode    = runtimeLogMode,
      edgePolicy        = edgePolicy,
      runtimeEdgePolicy = runtimeEdgePolicy,
      retiming          = retiming,
      addPipeStages     = addPipeStages,
      mulPipeStages     = mulPipeStages
    )
  }

  private def checkFrame[T <: Data: Real: BinaryRepresentation](
    params              : CFARParams[T],
    data                : Seq[Double],
    cfarMode            : CFARModel.CAMode,
    thresholdScale      : Double = 1.0,
    logMode             : Boolean = false,
    orderRankLeft       : Int = 2,
    orderRankRight      : Int = 2,
    edgePolicy          : Int = CFAREdgePolicy.SuppressEdges,
    peakGrouping        : Boolean = false,
    window              : CFARTestWindowCase = defaultWindow,
    readyPattern        : Seq[Boolean] = Seq(true),
    randomReadyValidSeed: Option[Long] = None
  ): Unit = {
    require(data.length == window.fftSize, s"Frame length ${data.length} must match fftSize ${window.fftSize}")

    test(new GOSCFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = cfarMode.value,
        thresholdScale = thresholdScale,
        logMode        = logMode,
        referenceCells = window.referenceCells,
        guardCells     = window.guardCells,
        orderRankLeft  = orderRankLeft,
        orderRankRight = orderRankRight,
        peakGrouping   = peakGrouping,
        edgePolicy     = edgePolicy,
        fftSize        = window.fftSize
      )
      expectFrame(
        dut,
        data,
        expectedGOSFrame(
          params,
          data,
          cfarMode,
          thresholdScale = thresholdScale,
          logMode        = logMode,
          referenceCells = window.referenceCells,
          guardCells     = window.guardCells,
          orderRankLeft  = orderRankLeft,
          orderRankRight = orderRankRight,
          edgePolicy     = edgePolicy,
          peakGrouping   = peakGrouping
        ),
        readyPattern         = readyPattern,
        randomReadyValidSeed = randomReadyValidSeed
      )
    }
  }

  for (lisType <- architectures) {
    it should s"rank single-reference one-sided windows with $lisType" in {
      val singleWindow = CFARTestWindowCase(8, 1, 1)
      val singleFrame = Seq(9.0, 1.0, 5.0, 2.0, 7.0, 3.0, 6.0, 4.0)
      checkFrame(
        params = gosParamsFor(
          UInt(8.W),
          lisType,
          window            = singleWindow,
          edgePolicy        = CFAREdgePolicy.OneSidedAverage,
          maxReferenceCells = 1,
          maxGuardCells     = 1
        ),
        data           = singleFrame,
        cfarMode       = CFARModel.GOSCA,
        orderRankLeft  = 1,
        orderRankRight = 1,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage,
        window         = singleWindow
      )
    }
  }

  it should "rank single-reference wraparound windows" in {
    val singleWindow = CFARTestWindowCase(8, 1, 1)
    val singleFrame  = Seq(9.0, 1.0, 5.0, 2.0, 7.0, 3.0, 6.0, 4.0)
    checkFrame(
      params = gosParamsFor(
        UInt(8.W),
        LISType.CntBased,
        window            = singleWindow,
        edgePolicy        = CFAREdgePolicy.WrapAroundFrame,
        maxReferenceCells = 1,
        maxGuardCells     = 1
      ),
      data           = singleFrame,
      cfarMode       = CFARModel.GOSGO,
      orderRankLeft  = 1,
      orderRankRight = 1,
      edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
      window         = singleWindow
    )
  }

  it should "rank wraparound windows without using inactive lanes" in {
    val runtimeWindow = CFARTestWindowCase(16, 2, 1)
    val frame = Seq(
      11.0, 5.0, 13.0, 7.0,
      17.0, 9.0, 19.0, 21.0,
      23.0, 25.0, 27.0, 29.0,
      31.0, 33.0, 35.0, 37.0
    )
    checkFrame(
      params = gosParamsFor(
        UInt(8.W),
        LISType.CntBased,
        window            = runtimeWindow,
        edgePolicy        = CFAREdgePolicy.WrapAroundFrame,
        maxReferenceCells = 4,
        maxGuardCells     = 2
      ),
      data = frame,
      cfarMode       = CFARModel.GOSSO,
      orderRankLeft  = 1,
      orderRankRight = runtimeWindow.referenceCells,
      edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
      window         = runtimeWindow
    )
  }

  for {
    lisType <- architectures
    mode <- CFARModel.gosModes
  } {
    it should s"compute ${mode.name} thresholds from selected side ranks with $lisType" in {
      checkFrame(
        params         = gosParamsFor(fixedType, lisType),
        data           = asymmetricFrame,
        cfarMode       = mode,
        orderRankLeft  = 2,
        orderRankRight = 2
      )
    }
  }

  for {
    lisType <- architectures
    rank <- Seq(1, 2, defaultWindow.referenceCells)
  } {
    it should s"select public order rank $rank with $lisType" in {
      checkFrame(
        params         = gosParamsFor(UInt(8.W), lisType),
        data           = asymmetricFrame,
        cfarMode       = CFARModel.GOSCA,
        orderRankLeft  = rank,
        orderRankRight = rank
      )
    }
  }

  for (lisType <- architectures) {
    it should s"exclude CUT and guard cells from GOS ranks with $lisType" in {
      checkFrame(
        params         = gosParamsFor(UInt(8.W), lisType),
        data           = guardAndCutStressFrame,
        cfarMode       = CFARModel.GOSGO,
        orderRankLeft  = 3,
        orderRankRight = 2
      )
    }
  }

  for (lisType <- architectures) {
    it should s"compare peak-grouping neighbors across the wrap boundary using $lisType" in {
      val wrapData = Seq(
        8.0, 1.0, 2.0, 1.0,
        7.0, 1.0, 6.0, 1.0,
        5.0, 1.0, 4.0, 1.0,
        3.0, 1.0, 2.0, 9.0
      )
      val wrapParams = gosParamsFor(fixedType, lisType, edgePolicy = CFAREdgePolicy.WrapAroundFrame)
      val wrapExpected = expectedGOSFrame(
        wrapParams,
        wrapData,
        CFARModel.GOSCA,
        thresholdScale = 0.0,
        referenceCells = defaultWindow.referenceCells,
        guardCells     = defaultWindow.guardCells,
        orderRankLeft  = 2,
        orderRankRight = 3,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
        peakGrouping   = true
      )
      assert(!wrapExpected.head.peak, "wrap bin 0 must compare against bin N-1")
      checkFrame(
        params = wrapParams,
        data           = wrapData,
        cfarMode       = CFARModel.GOSCA,
        thresholdScale = 0.0,
        orderRankLeft  = 2,
        orderRankRight = 3,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
        peakGrouping   = true
      )
    }
  }

  for {
    lisType <- architectures
    mode <- CFARModel.gosModes
  } {
    it should s"use one-sided ranks for edge bins in ${mode.name} using $lisType" in {
      val params = gosParamsFor(fixedType, lisType, edgePolicy = CFAREdgePolicy.OneSidedAverage)
      val expected = expectedGOSFrame(
        params,
        asymmetricFrame,
        mode,
        referenceCells = defaultWindow.referenceCells,
        guardCells     = defaultWindow.guardCells,
        orderRankLeft  = 2,
        orderRankRight = 3,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage
      )
      assert(expected.head.threshold.raw != 0, "one-sided leading edge should produce a right-side GOS threshold")
      assert(expected.last.threshold.raw != 0, "one-sided trailing edge should produce a left-side GOS threshold")

      checkFrame(
        params         = params,
        data           = asymmetricFrame,
        cfarMode       = mode,
        orderRankLeft  = 2,
        orderRankRight = 3,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage
      )
    }
  }

  for {
    lisType <- architectures
    mode <- CFARModel.gosModes
  } {
    it should s"wrap GOS reference ranks inside each frame in ${mode.name} using $lisType" in {
      checkFrame(
        params         = gosParamsFor(fixedType, lisType, edgePolicy = CFAREdgePolicy.WrapAroundFrame, mulPipeStages = 1),
        data           = asymmetricFrame,
        cfarMode       = mode,
        orderRankLeft  = 2,
        orderRankRight = 3,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
        readyPattern   = Seq(true, false, true, true, false, true)
      )
    }
  }

  for {
    lisType <- architectures
    mode <- CFARModel.gosModes
  } {
    it should s"avoid detections on constant noise in ${mode.name} using $lisType" in {
      val constantFrame = Seq.fill(defaultWindow.fftSize)(4.0)
      val params = gosParamsFor(UInt(8.W), lisType)
      val expected = expectedGOSFrame(
        params,
        constantFrame,
        mode,
        thresholdScale = 2.0,
        referenceCells = defaultWindow.referenceCells,
        guardCells     = defaultWindow.guardCells,
        orderRankLeft  = 2,
        orderRankRight = 2
      )
      assert(expected.forall(!_.peak), "constant-noise GOS frame should not detect with threshold scale > 1")

      checkFrame(
        params         = params,
        data           = constantFrame,
        cfarMode       = mode,
        thresholdScale = 2.0,
        orderRankLeft  = 2,
        orderRankRight = 2
      )
    }
  }

  for (lisType <- architectures) {
    it should s"apply asymmetric left and right ranks on a reversed frame with $lisType" in {
      val reverseAsymmetricFrame = asymmetricFrame.reverse
      checkFrame(
        params         = gosParamsFor(fixedType, lisType),
        data           = reverseAsymmetricFrame,
        cfarMode       = CFARModel.GOSGO,
        orderRankLeft  = 3,
        orderRankRight = 1
      )
    }
  }

  for (lisType <- architectures) {
    it should s"truncate odd GOS-CA rank averages with $lisType" in {
      val oddAverageFrame = Seq(
        5.0, 20.0, 30.0, 40.0,
        1.0, 100.0, 1.0, 8.0,
        50.0, 60.0, 70.0, 4.0,
        4.0, 4.0, 4.0, 4.0
      )
      val params = gosParamsFor(UInt(8.W), lisType)
      val expected = expectedGOSFrame(
        params,
        oddAverageFrame,
        CFARModel.GOSCA,
        referenceCells = defaultWindow.referenceCells,
        guardCells     = defaultWindow.guardCells,
        orderRankLeft  = 1,
        orderRankRight = 1
      )
      assert(expected(defaultWindow.edgeSpan).trace.selectedAverage == 6)

      checkFrame(
        params         = params,
        data           = oddAverageFrame,
        cfarMode       = CFARModel.GOSCA,
        orderRankLeft  = 1,
        orderRankRight = 1
      )
    }
  }

  for (lisType <- architectures) {
    it should s"avoid false detections at maximum UInt input values with $lisType" in {
      val maxFrame = Seq.fill(defaultWindow.fftSize)(255.0)
      val params = gosParamsFor(UInt(8.W), lisType)
      val expected = expectedGOSFrame(
        params,
        maxFrame,
        CFARModel.GOSCA,
        referenceCells = defaultWindow.referenceCells,
        guardCells     = defaultWindow.guardCells,
        orderRankLeft  = defaultWindow.referenceCells,
        orderRankRight = defaultWindow.referenceCells
      )
      assert(expected.forall(!_.peak), "maximum constant UInt frame should not self-detect at scale 1")

      checkFrame(
        params         = params,
        data           = maxFrame,
        cfarMode       = CFARModel.GOSCA,
        orderRankLeft  = defaultWindow.referenceCells,
        orderRankRight = defaultWindow.referenceCells
      )
    }
  }

  for (lisType <- architectures) {
    it should s"align thresholds and peaks when CUT output is disabled with $lisType" in {
      checkFrame(
        params         = gosParamsFor(fixedType, lisType, sendCut = false),
        data           = asymmetricFrame,
        cfarMode       = CFARModel.GOSGO,
        orderRankLeft  = 2,
        orderRankRight = 3
      )
    }
  }

  for (lisType <- architectures) {
    it should s"align GOS outputs through multiplier pipeline stages with $lisType" in {
      checkFrame(
        params         = gosParamsFor(fixedType, lisType, mulPipeStages = 1),
        data           = asymmetricFrame,
        cfarMode       = CFARModel.GOSGO,
        thresholdScale = 1.25,
        orderRankLeft  = 2,
        orderRankRight = 2,
        readyPattern   = Seq(true, false, true, true, false, true)
      )
    }
  }

  for (lisType <- architectures) {
    it should s"latch GOS runtime controls only at frame boundaries using $lisType" in {
      val params = gosParamsFor(fixedType, lisType, runtimeLogMode = true, addPipeStages = 1, mulPipeStages = 1)

      test(new GOSCFAR(params)).withAnnotations(annotations) { dut =>
        configure(
          dut,
          cfarMode       = CFARMode.CellAveraging,
          thresholdScale = 1.0,
          logMode        = false,
          referenceCells = defaultWindow.referenceCells,
          guardCells     = defaultWindow.guardCells,
          orderRankLeft  = 1,
          orderRankRight = 2,
          fftSize        = defaultWindow.fftSize
        )
        expectFrame(
          dut,
          asymmetricFrame,
          expectedGOSFrame(
            params,
            asymmetricFrame,
            CFARModel.GOSCA,
            thresholdScale = 1.0,
            logMode        = false,
            referenceCells = defaultWindow.referenceCells,
            guardCells     = defaultWindow.guardCells,
            orderRankLeft  = 1,
            orderRankRight = 2
          ),
          onInputAccepted = { (sampleIndex: Int, activeDut: GOSCFAR[FixedPoint]) =>
            sampleIndex match {
            case 3 =>
              configure(
                activeDut,
                cfarMode       = CFARMode.GreatestOf,
                thresholdScale = 1.25,
                logMode        = true,
                referenceCells = defaultWindow.referenceCells,
                guardCells     = defaultWindow.guardCells,
                orderRankLeft  = 3,
                orderRankRight = 4,
                edgePolicy     = CFAREdgePolicy.SuppressEdges,
                fftSize        = defaultWindow.fftSize
              )
            case _ =>
            }
          }
        )

        configure(
          dut,
          cfarMode       = CFARMode.GreatestOf,
          thresholdScale = 1.25,
          logMode        = true,
          referenceCells = defaultWindow.referenceCells,
          guardCells     = defaultWindow.guardCells,
          orderRankLeft  = 3,
          orderRankRight = 4,
          fftSize        = defaultWindow.fftSize
        )
        expectFrame(
          dut,
          duplicateFrame,
          expectedGOSFrame(
            params,
            duplicateFrame,
            CFARModel.GOSGO,
            thresholdScale = 1.25,
            logMode        = true,
            referenceCells = defaultWindow.referenceCells,
            guardCells     = defaultWindow.guardCells,
            orderRankLeft  = 3,
            orderRankRight = 4
          )
        )
      }
    }
  }

  for (lisType <- architectures) {
    it should s"apply runtime edge policy with retiming and log-mode changes using $lisType" in {
      val params = gosParamsFor(
        fixedType,
        lisType,
        runtimeLogMode    = true,
        runtimeEdgePolicy = true,
        retiming          = true,
        addPipeStages     = 1,
        mulPipeStages     = 1
      )
      val frames = Seq(
        (asymmetricFrame, CFARModel.GOSCA, CFAREdgePolicy.SuppressEdges, 1, 2, 1.0, false),
        (duplicateFrame, CFARModel.GOSGO, CFAREdgePolicy.OneSidedAverage, 2, 3, 1.25, false),
        (guardAndCutStressFrame, CFARModel.GOSSO, CFAREdgePolicy.WrapAroundFrame, 3, 2, 1.0, true)
      )

      test(new GOSCFAR(params)).withAnnotations(annotations) { dut =>
        frames.foreach {
          case (frame, mode, edgePolicy, leftRank, rightRank, thresholdScale, logMode) =>
            configure(
              dut,
              cfarMode       = mode.value,
              thresholdScale = thresholdScale,
              logMode        = logMode,
              referenceCells = defaultWindow.referenceCells,
              guardCells     = defaultWindow.guardCells,
              orderRankLeft  = leftRank,
              orderRankRight = rightRank,
              edgePolicy     = edgePolicy,
              fftSize        = defaultWindow.fftSize
            )
            expectFrame(
              dut,
              frame,
              expectedGOSFrame(
                params,
                frame,
                mode,
                thresholdScale = thresholdScale,
                logMode        = logMode,
                referenceCells = defaultWindow.referenceCells,
                guardCells     = defaultWindow.guardCells,
                orderRankLeft  = leftRank,
                orderRankRight = rightRank,
                edgePolicy     = edgePolicy
              ),
              readyPattern = Seq(true, false, true, true, false, true)
            )
        }
      }
    }
  }

  for {
    lisType <- architectures
    edgePolicy <- allEdgePolicies
  } {
    it should s"match GOS outputs under randomized ready-valid using $lisType and edge policy $edgePolicy" in {
      checkFrame(
        params               = gosParamsFor(fixedType, lisType, edgePolicy = edgePolicy, mulPipeStages = 1),
        data                 = asymmetricFrame,
        cfarMode             = CFARModel.GOSCA,
        orderRankLeft        = 2,
        orderRankRight       = 3,
        edgePolicy           = edgePolicy,
        randomReadyValidSeed = Some(0x605CFAFL + lisType.length + edgePolicy)
      )
    }
  }

  for (lisType <- architectures) {
    it should s"hold GOS output bits stable while backpressured with $lisType" in {
      val params = gosParamsFor(fixedType, lisType, mulPipeStages = 1)

      test(new GOSCFAR(params)).withAnnotations(annotations) { dut =>
        configure(
          dut,
          cfarMode       = CFARMode.GreatestOf,
          referenceCells = defaultWindow.referenceCells,
          guardCells     = defaultWindow.guardCells,
          orderRankLeft  = 2,
          orderRankRight = 2,
          fftSize        = defaultWindow.fftSize
        )
        expectOutputStableWhileBackpressured(dut, asymmetricFrame)
      }
    }
  }

  it should "preserve wraparound frame outputs after a long output stall" in {
    checkFrame(
      params = gosParamsFor(
        fixedType,
        LISType.CntBased,
        edgePolicy    = CFAREdgePolicy.WrapAroundFrame,
        mulPipeStages = 1
      ),
      data           = asymmetricFrame,
      cfarMode       = CFARModel.GOSCA,
      orderRankLeft  = 2,
      orderRankRight = 3,
      edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
      readyPattern   = Seq.fill(80)(false) ++ Seq.fill(16)(true)
    )
  }

  it should "preserve one-sided frame outputs after a long output stall" in {
    checkFrame(
      params = gosParamsFor(
        fixedType,
        LISType.CntBased,
        edgePolicy = CFAREdgePolicy.OneSidedAverage,
        mulPipeStages = 1
      ),
      data           = asymmetricFrame,
      cfarMode       = CFARModel.GOSGO,
      orderRankLeft  = 2,
      orderRankRight = 3,
      edgePolicy     = CFAREdgePolicy.OneSidedAverage,
      readyPattern   = Seq.fill(80)(false) ++ Seq.fill(16)(true)
    )
  }

  it should "backpressure input when frame buffers fill under output stall" in {
    val params = gosParamsFor(
      fixedType,
      LISType.CntBased,
      edgePolicy = CFAREdgePolicy.WrapAroundFrame,
      mulPipeStages = 1
    )

    test(new GOSCFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.CellAveraging,
        referenceCells = defaultWindow.referenceCells,
        guardCells     = defaultWindow.guardCells,
        orderRankLeft  = 2,
        orderRankRight = 3,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
        fftSize        = defaultWindow.fftSize
      )
      expectInputBackpressureWhenFrameBuffersFill(dut, asymmetricFrame)
    }
  }
}
