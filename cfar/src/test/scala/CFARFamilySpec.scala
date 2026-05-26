package opera.cfar

import chisel3._
import chiseltest.ChiselScalatestTester
import fixedpoint._
import org.scalatest.flatspec.AnyFlatSpec
import opera.cfar.CFARModel.expectedFrame
import opera.cfar.CFARTester.{configure, expectFrame, expectOutputStableWhileBackpressured}

class CFARFamilySpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "Cell Averaging CFAR family"

  private val fixedType = FixedPoint(16.W, 6.BP)
  private val defaultWindow = CFARTestWindowCase(16, 2, 1)
  private val edgeWindowCases = Seq(
    CFARTestWindowCase(16, 1, 1),
    CFARTestWindowCase(16, 2, 1),
    CFARTestWindowCase(16, 4, 1),
    CFARTestWindowCase(16, 4, 2),
    CFARTestWindowCase(32, 8, 2)
  )
  private val randomWindowCases = Seq(
    CFARTestWindowCase(16, 2, 1),
    CFARTestWindowCase(32, 4, 2)
  )
  private val randomSeeds = Seq(0xC00FFEE1L, 0xC00FFEE2L)
  private val asymmetricSeed = Seq(
    1.00, 2.00, 4.00, 7.00,
    24.00, 5.00, 6.00, 8.00,
    3.00, 14.00, 4.00, 6.00,
    32.00, 5.00, 7.00, 9.00
  )
  private def frameFor(window: CFARTestWindowCase): Seq[Double] =
    (0 until window.fftSize).map { index =>
      asymmetricSeed(index % asymmetricSeed.length) + 3.0 * (index / asymmetricSeed.length)
    }

  private val asymmetricFrame = frameFor(defaultWindow)
  private val peakFrame = Seq(
    2.00 , 2.00 , 3.00, 4.00,
    40.00, 5.00 , 4.00, 3.00,
    2.00 , 28.00, 3.00, 4.00,
    36.00, 5.00 , 4.00, 3.00
  )

  private val oneSidedParams      = paramsForWindow(fixedType, defaultWindow, edgePolicy = CFAREdgePolicy.OneSidedAverage)
  private val suppressEdgesParams = paramsForWindow(fixedType, defaultWindow, edgePolicy = CFAREdgePolicy.SuppressEdges)
  private val wrapAroundParams    = paramsForWindow(fixedType, defaultWindow, edgePolicy = CFAREdgePolicy.WrapAroundFrame)

  /**
   * Runs one frame with the standard small CFAR window used by these tests.
   *
   * Keeps the tests focused on the selected CA-family mode or edge policy instead of repeating the Chisel handshaking code.
   */
  private def checkFrame(
    params: CFARParams[FixedPoint],
    data: Seq[Double],
    cfarMode: CFARModel.CAMode,
    thresholdScale: Double = 1.0,
    logMode: Boolean = false,
    edgePolicy: Int = CFAREdgePolicy.OneSidedAverage,
    peakGrouping: Boolean = false,
    window: CFARTestWindowCase = defaultWindow,
    readyPattern: Seq[Boolean] = Seq(true),
    randomReadyValidSeed: Option[Long] = None
  ): Unit = {
    require(data.length == window.fftSize, s"Frame length ${data.length} must match fftSize ${window.fftSize}")

    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode = cfarMode.value,
        thresholdScale = thresholdScale,
        logMode = logMode,
        referenceCells = window.referenceCells,
        guardCells = window.guardCells,
        peakGrouping = peakGrouping,
        edgePolicy = edgePolicy,
        fftSize = window.fftSize
      )
      expectFrame(
        dut,
        data,
        Some(expectedFrame(
          params,
          data,
          cfarMode,
          thresholdScale = thresholdScale,
          logMode = logMode,
          referenceCells = window.referenceCells,
          guardCells = window.guardCells,
          edgePolicy = edgePolicy,
          peakGrouping = peakGrouping
        )),
        readyPattern = readyPattern,
        randomReadyValidSeed = randomReadyValidSeed
      )
    }
  }

  /**
   * Classical CA-CFAR combines both sides by averaging the already-averaged left and right reference windows.
   */
  it should "average both reference sides in Classical CA mode" in {
    checkFrame(oneSidedParams, asymmetricFrame, CFARModel.ClassicalCA)
  }

  /**
   * GOCA-CFAR is selected with the same datapath but chooses the larger side average before threshold scaling.
   */
  it should "select the larger side average in GOCA mode" in {
    checkFrame(oneSidedParams, asymmetricFrame, CFARModel.GOCA)
  }

  /**
   * SOCA-CFAR is selected with the same datapath but chooses the smaller side average before threshold scaling.
   */
  it should "select the smaller side average in SOCA mode" in {
    checkFrame(oneSidedParams, asymmetricFrame, CFARModel.SOCA)
  }

  /**
   * Peak tests check the exact boolean output, not only the threshold number.
   */
  it should "produce expected peak bits for every CA-family mode" in {
    for (mode <- CFARModel.caModes) {
      checkFrame(oneSidedParams, peakFrame, mode)
    }
  }

  /**
   * OneSidedAverage keeps one output per bin. Left-edge bins use the right-side average.
   * Right-edge bins use the left-side average. Middle bins use the selected CA-family mode.
   */
  it should "handle edge bins with one-sided averages for every CA-family mode" in {
    for {
      window <- edgeWindowCases
      mode <- CFARModel.caModes
    } {
      checkFrame(
        paramsForWindow(fixedType, window, edgePolicy = CFAREdgePolicy.OneSidedAverage),
        frameFor(window),
        mode,
        window = window
      )
    }
  }

  /**
   * SuppressEdges clears detections only where a full two-sided window is not available.
   */
  it should "clear edge thresholds and peaks with SuppressEdges" in {
    for {
      window <- edgeWindowCases
      mode <- CFARModel.caModes
    } {
      val params = paramsForWindow(fixedType, window, edgePolicy = CFAREdgePolicy.SuppressEdges)
      val data = frameFor(window)
      val expected = expectedFrame(
        params,
        data,
        mode,
        referenceCells = window.referenceCells,
        guardCells = window.guardCells,
        edgePolicy = CFAREdgePolicy.SuppressEdges
      )
      assert(expected.take(window.edgeSpan).forall(bin => bin.threshold.raw == 0 && !bin.peak))
      assert(expected.takeRight(window.edgeSpan).forall(bin => bin.threshold.raw == 0 && !bin.peak))
      checkFrame(params, data, mode, edgePolicy = CFAREdgePolicy.SuppressEdges, window = window)
    }
  }

  /**
   * WrapAroundFrame uses circular references from the same FFT frame.
   * Bin 0 use tail samples and the final bins use head samples.
   */
  it should "wrap reference windows inside the same frame for every CA-family mode" in {
    for {
      window <- edgeWindowCases
      mode <- CFARModel.caModes
    } {
      checkFrame(
        paramsForWindow(fixedType, window, edgePolicy = CFAREdgePolicy.WrapAroundFrame),
        frameFor(window),
        mode,
        edgePolicy = CFAREdgePolicy.WrapAroundFrame,
        window = window
      )
    }
  }

  it should "apply edge-policy neighbor semantics for CA peak grouping" in {
    val wrapData = Seq(
      8.0, 1.0, 2.0, 1.0,
      7.0, 1.0, 6.0, 1.0,
      5.0, 1.0, 4.0, 1.0,
      3.0, 1.0, 2.0, 9.0
    )
    val linearData = Seq(
      8.0, 1.0, 2.0, 1.0,
      7.0, 1.0, 6.0, 1.0,
      5.0, 1.0, 4.0, 1.0,
      3.0, 1.0, 2.0, 1.0
    )

    val wrapExpected = expectedFrame(
      wrapAroundParams,
      wrapData,
      CFARModel.ClassicalCA,
      thresholdScale = 0.0,
      referenceCells = defaultWindow.referenceCells,
      guardCells = defaultWindow.guardCells,
      edgePolicy = CFAREdgePolicy.WrapAroundFrame,
      peakGrouping = true
    )
    assert(!wrapExpected.head.peak, "wrap bin 0 must compare against bin N-1")
    checkFrame(
      wrapAroundParams,
      wrapData,
      CFARModel.ClassicalCA,
      thresholdScale = 0.0,
      edgePolicy = CFAREdgePolicy.WrapAroundFrame,
      peakGrouping = true
    )

    val oneSidedExpected = expectedFrame(
      oneSidedParams,
      linearData,
      CFARModel.ClassicalCA,
      thresholdScale = 0.0,
      referenceCells = defaultWindow.referenceCells,
      guardCells = defaultWindow.guardCells,
      edgePolicy = CFAREdgePolicy.OneSidedAverage,
      peakGrouping = true
    )
    assert(oneSidedExpected.head.peak, "linear bin 0 must ignore the missing previous neighbor")
    checkFrame(
      oneSidedParams,
      linearData,
      CFARModel.ClassicalCA,
      thresholdScale = 0.0,
      edgePolicy = CFAREdgePolicy.OneSidedAverage,
      peakGrouping = true
    )

    val suppressExpected = expectedFrame(
      suppressEdgesParams,
      linearData,
      CFARModel.ClassicalCA,
      thresholdScale = 0.0,
      referenceCells = defaultWindow.referenceCells,
      guardCells = defaultWindow.guardCells,
      edgePolicy = CFAREdgePolicy.SuppressEdges,
      peakGrouping = true
    )
    assert(suppressExpected.take(defaultWindow.edgeSpan).forall(!_.peak), "suppress leading edges must not peak")
    assert(suppressExpected.takeRight(defaultWindow.edgeSpan).forall(!_.peak), "suppress trailing edges must not peak")
    checkFrame(
      suppressEdgesParams,
      linearData,
      CFARModel.ClassicalCA,
      thresholdScale = 0.0,
      edgePolicy = CFAREdgePolicy.SuppressEdges,
      peakGrouping = true
    )
  }

  /**
   * Randomized ready/valid tests for every CA-family mode and edge policy.
   */
  it should "preserve CA-family outputs with randomized ready-valid for every edge policy" in {
    for {
      mode <- CFARModel.caModes
      edgePolicy <- allEdgePolicies
      window <- randomWindowCases
      seedBase <- randomSeeds
    } {
      val seed = seedBase + mode.value.toLong * 64L + edgePolicy.toLong * 8L + window.fftSize.toLong

      checkFrame(
        paramsForWindow(fixedType, window, edgePolicy = edgePolicy, mulPipeStages = 1),
        frameFor(window),
        mode,
        edgePolicy = edgePolicy,
        window = window,
        randomReadyValidSeed = Some(seed)
      )
    }
  }

  /**
   * Backpressure must not change threshold, peak, CUT, FFT-bin, or last-bit alignment.
   */
  it should "preserve CA-family outputs under backpressure" in {
    checkFrame(
      oneSidedParams,
      peakFrame,
      CFARModel.GOCA,
      readyPattern = Seq(true, true, false, true, false, true, true)
    )
  }

  /**
   * Decoupled output payloads must remain stable while the downstream block holds `ready` low. 
   */
  it should "hold output bits stable while backpressured" in {
    val params = paramsFor(
      fixedType,
      maxFftSize = defaultWindow.fftSize,
      edgePolicy = CFAREdgePolicy.OneSidedAverage,
      mulPipeStages = 1
    )

    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode = CFARMode.GreatestOf,
        referenceCells = defaultWindow.referenceCells,
        guardCells = defaultWindow.guardCells,
        fftSize = defaultWindow.fftSize
      )
      expectOutputStableWhileBackpressured(dut, asymmetricFrame)
    }
  }

  /**
   * Pipeline-stage tests keep the FixedPoint multiplier path active while checking threshold, peak, CUT, bin, and last-bit metadata alignment.
   */
  it should "preserve metadata alignment through threshold pipeline stages" in {
    val frame = (0 until defaultFftSize).map(_.toDouble)
    val params = paramsFor(fixedType, mulPipeStages = 1)

    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.GreatestOf)
      expectFrame(
        dut,
        frame,
        Some(expectedFrame(params, frame, CFARModel.GOCA)),
        readyPattern = Seq(true, false, true, true, false, true)
      )
    }
  }

  /**
   * Small runtime windows, one-sided edge averaging, and a multiplier pipeline must keep threshold and metadata aligned under backpressure.
   */
  it should "handle the small-window one-sided pipeline case" in {
    for (thresholdScale <- Seq(0.5, 0.75, 1.0, 1.25, 1.5, 2.0)) {
      val params = paramsForWindow(fixedType, defaultWindow, edgePolicy = CFAREdgePolicy.OneSidedAverage, mulPipeStages = 1)

      test(new CFAR(params)).withAnnotations(annotations) { dut =>
        configure(
          dut,
          cfarMode = CFARMode.SmallestOf,
          thresholdScale = thresholdScale,
          referenceCells = defaultWindow.referenceCells,
          guardCells = defaultWindow.guardCells,
          edgePolicy = CFAREdgePolicy.OneSidedAverage,
          fftSize = defaultWindow.fftSize
        )
        expectFrame(
          dut,
          asymmetricFrame,
          Some(expectedFrame(
            params,
            asymmetricFrame,
            CFARModel.SOCA,
            thresholdScale = thresholdScale,
            referenceCells = defaultWindow.referenceCells,
            guardCells = defaultWindow.guardCells,
            edgePolicy = CFAREdgePolicy.OneSidedAverage
          )),
          readyPattern = Seq(true, false, true, true, false, true)
        )
      }
    }
  }
}
