package opera.cfar

import chisel3._
import chiseltest.ChiselScalatestTester
import dsptools.numbers._
import fixedpoint._
import org.scalatest.flatspec.AnyFlatSpec
import opera.cfar.CFARModel.expectedFrame
import opera.cfar.CFARTester.{
  configure,
  expectFrame,
  expectInputBackpressureWhenFrameBuffersFill,
  expectOutputStableWhileBackpressured
}

class CFARSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "CFAR"

  private val fftSize            = defaultFftSize
  private val fixedType          = FixedPoint(16.W, 6.BP)
  private val fixedInput         = (0 until fftSize).map(_.toDouble)
  private val integerInput       = (0 until fftSize).map(i => (2 * i).toDouble)
  private val caModes            = CFARModel.caModes
  private val edgeReferenceCells = 2
  private val edgeGuardCells     = 1

  private def addThresholdTests[T <: Data: Real: BinaryRepresentation](
    typeName: String,
    dataType: T,
    data    : Seq[Double]
  ): Unit = {
    for (mode <- caModes) {
      /**
       * Verifies that each sample format uses the same raw averaging and threshold comparison rules as the bit-accurate model.
       */
      it should s"match ${mode.name} thresholds for $typeName" in {
        val params = paramsFor(dataType)
        test(new CFAR(params)).withAnnotations(annotations) { dut =>
          configure(dut, cfarMode = mode.value)
          expectFrame(dut, data, Some(expectedFrame(params, data, mode)))
        }
      }
    }
  }

  addThresholdTests("UInt(8.W)"             , UInt(8.W)             , integerInput)
  addThresholdTests("UInt(16.W)"            , UInt(16.W)            , integerInput)
  addThresholdTests("SInt(8.W)"             , SInt(8.W)             , integerInput)
  addThresholdTests("SInt(16.W)"            , SInt(16.W), integerInput)
  addThresholdTests("FixedPoint(12.W, 4.BP)", FixedPoint(12.W, 4.BP), fixedInput)
  addThresholdTests("FixedPoint(16.W, 6.BP)", FixedPoint(16.W, 6.BP), fixedInput)
  addThresholdTests("FixedPoint(18.W, 8.BP)", FixedPoint(18.W, 8.BP), fixedInput)

  /**
   * Verifies that disabling `sendCut` does not disturb threshold, peak, FFT-bin, or last alignment.
   */
  it should "produce aligned outputs when cut output is disabled" in {
    val params = paramsFor(fixedType, sendCut = false)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.CellAveraging)
      expectFrame(dut, fixedInput, Some(expectedFrame(params, fixedInput, CFARModel.ClassicalCA)))
    }
  }

  /**
   * Stalls the output interface after the stream is active to catch payload alignment bugs in the Decoupled ready/valid path.
   */
  it should "preserve thresholds and peaks under output backpressure" in {
    val params = paramsFor(fixedType)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.GreatestOf)
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(params, fixedInput, CFARModel.GOCA)),
        readyPattern = Seq.fill(12)(true) ++ Seq(false, true, false, true)
      )
    }
  }

  /**
   * Forces the reference delay helper onto its SRAM-backed implementation and checks that the top-level CFAR thresholds remain bit accurate.
   */
  it should "match thresholds with SRAM-backed reference delays" in {
    val params = paramsFor(fixedType, minSRAMDepth = 4)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.GreatestOf)
      expectFrame(dut, fixedInput, Some(expectedFrame(params, fixedInput, CFARModel.GOCA)))
    }
  }

  /**
   * Loads a new stream-core configuration in the middle of one frame and proves it is held as pending state until the following frame starts.
   */
  it should "sample stream runtime controls only at frame boundaries" in {
    val params               = paramsFor(fixedType)
    val firstReferenceCells  = 4
    val firstGuardCells      = 1
    val secondReferenceCells = 2
    val secondGuardCells     = 1
    val secondFrame          = fixedInput.map(_ + 20.0)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.GreatestOf,
        referenceCells = firstReferenceCells,
        guardCells     = firstGuardCells
      )
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(
          params,
          fixedInput,
          CFARModel.GOCA,
          referenceCells = firstReferenceCells,
          guardCells = firstGuardCells
        )),
        onInputAccepted = { (sampleIndex: Int, activeDut: CFAR[FixedPoint]) =>
          if (sampleIndex == 4) {
            configure(
              activeDut,
              cfarMode = CFARMode.SmallestOf,
              referenceCells = secondReferenceCells,
              guardCells = secondGuardCells
            )
          }
        }
      )
      expectFrame(
        dut,
        secondFrame,
        Some(expectedFrame(
          params,
          secondFrame,
          CFARModel.SOCA,
          referenceCells = secondReferenceCells,
          guardCells = secondGuardCells
        ))
      )
    }
  }

  /**
   * Checks static log-mode threshold scaling.
   * The threshold scale is added in the log domain instead of multiplied in the linear domain.
   */
  it should "use additive threshold scaling in static log mode" in {
    val params = paramsFor(fixedType, logMode = true, addPipeStages = 1)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.CellAveraging, thresholdScale = 1.0)
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(
          params,
          fixedInput,
          CFARModel.ClassicalCA,
          thresholdScale = 1.0,
          logMode = true
        ))
      )
    }
  }

  /**
   * Selects the linear threshold path through the runtime log-mode control while both add and multiply pipeline options are generated.
   */
  it should "select linear threshold scaling at runtime" in {
    val params = paramsFor(fixedType, runtimeLogMode = true, addPipeStages = 1, mulPipeStages = 1)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.SmallestOf, thresholdScale = 1.0, logMode = false)
      expectFrame(dut, fixedInput, Some(expectedFrame(params, fixedInput, CFARModel.SOCA)))
    }
  }

  /**
   * Selects the additive log threshold path through the runtime log-mode control and compares the result to the bit-accurate model.
   */
  it should "select log threshold scaling at runtime" in {
    val params = paramsFor(fixedType, runtimeLogMode = true, addPipeStages = 1, mulPipeStages = 1)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(dut, cfarMode = CFARMode.SmallestOf, thresholdScale = 1.0, logMode = true)
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(
          params,
          fixedInput,
          CFARModel.SOCA,
          thresholdScale = 1.0,
          logMode = true
        ))
      )
    }
  }

  /**
   * Combines WrapAroundFrame with the multiplier pipeline and output stalls to catch alignment mistakes in the buffered frame path.
   */
  it should "align wraparound outputs through multiplier pipeline stages" in {
    val params = paramsFor(fixedType, edgePolicy = CFAREdgePolicy.WrapAroundFrame, mulPipeStages = 1)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.GreatestOf,
        referenceCells = edgeReferenceCells,
        guardCells     = edgeGuardCells
      )
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(
          params,
          fixedInput,
          CFARModel.GOCA,
          referenceCells = edgeReferenceCells,
          guardCells     = edgeGuardCells,
          edgePolicy     = CFAREdgePolicy.WrapAroundFrame
        )),
        readyPattern = Seq(true, true, false, true, true, false, true)
      )
    }
  }

  /**
   * Holds the frame-buffered wraparound output stalled and checks that every visible payload bit stays stable for CA, GOCA, and SOCA mode selections.
   */
  it should "hold wraparound output bits stable while backpressured for every CA-family mode" in {
    for (mode <- caModes) {
      val params = paramsFor(fixedType, edgePolicy = CFAREdgePolicy.WrapAroundFrame, mulPipeStages = 1)
      test(new CFAR(params)).withAnnotations(annotations) { dut =>
        configure(
          dut,
          cfarMode       = mode.value,
          referenceCells = edgeReferenceCells,
          guardCells     = edgeGuardCells
        )
        expectOutputStableWhileBackpressured(
          dut,
          fixedInput,
          stableCycles = 3,
          maxCycles = 1000
        )
      }
    }
  }

  it should "preserve frame outputs after a long output stall" in {
    val params = paramsFor(fixedType, edgePolicy = CFAREdgePolicy.WrapAroundFrame, mulPipeStages = 1)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.CellAveraging,
        referenceCells = edgeReferenceCells,
        guardCells     = edgeGuardCells,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame
      )
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(
          params,
          fixedInput,
          CFARModel.ClassicalCA,
          referenceCells = edgeReferenceCells,
          guardCells = edgeGuardCells,
          edgePolicy = CFAREdgePolicy.WrapAroundFrame
        )),
        readyPattern = Seq.fill(80)(false) ++ Seq.fill(16)(true)
      )
    }
  }

  it should "deassert input ready when frame buffers fill under output stall" in {
    val params = paramsFor(fixedType, edgePolicy = CFAREdgePolicy.WrapAroundFrame, mulPipeStages = 1)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.CellAveraging,
        referenceCells = edgeReferenceCells,
        guardCells     = edgeGuardCells,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame
      )
      expectInputBackpressureWhenFrameBuffersFill(dut, fixedInput)
    }
  }

  /**
   * Generates the runtime edge-policy input and checks that each policy value chooses the expected edge behavior.
   */
  it should "select edge policy at runtime" in {
    for (edgePolicy <- allEdgePolicies) {
      val params = paramsFor(fixedType, runtimeEdgePolicy = true)
      test(new CFAR(params)).withAnnotations(annotations) { dut =>
        configure(
          dut,
          cfarMode = CFARMode.CellAveraging,
          referenceCells = edgeReferenceCells,
          guardCells = edgeGuardCells,
          edgePolicy = edgePolicy
        )
        expectFrame(
          dut,
          fixedInput,
          Some(expectedFrame(
            params,
            fixedInput,
            CFARModel.ClassicalCA,
            referenceCells = edgeReferenceCells,
            guardCells     = edgeGuardCells,
            edgePolicy = edgePolicy
          ))
        )
      }
    }
  }

  /**
   * Repeats the frame-boundary configuration check on the buffered path used by runtime edge policy and wrap-around windows.
   */
  it should "sample frame-buffered runtime controls only at frame boundaries" in {
    val params      = paramsFor(fixedType, runtimeEdgePolicy = true)
    val firstFrame  = fixedInput
    val secondFrame = fixedInput.map(_ + 50.0)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.CellAveraging,
        referenceCells = edgeReferenceCells,
        guardCells     = edgeGuardCells,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame
      )
      expectFrame(
        dut,
        firstFrame,
        Some(expectedFrame(
          params,
          firstFrame,
          CFARModel.ClassicalCA,
          referenceCells = edgeReferenceCells,
          guardCells = edgeGuardCells,
          edgePolicy = CFAREdgePolicy.WrapAroundFrame
        )),
        onInputAccepted = { (sampleIndex: Int, activeDut: CFAR[FixedPoint]) =>
          if (sampleIndex == 4) {
            configure(
              activeDut,
              cfarMode       = CFARMode.GreatestOf,
              referenceCells = edgeReferenceCells,
              guardCells     = edgeGuardCells,
              edgePolicy     = CFAREdgePolicy.SuppressEdges
            )
          }
        }
      )
      expectFrame(
        dut,
        secondFrame,
        Some(expectedFrame(
          params,
          secondFrame,
          CFARModel.GOCA,
          referenceCells = edgeReferenceCells,
          guardCells     = edgeGuardCells,
          edgePolicy     = CFAREdgePolicy.SuppressEdges
        ))
      )
    }
  }

  /**
   * Exercises runtime edge policy, runtime log mode, retiming, and threshold pipeline options together to catch any unexpected interactions in the logic or timing of these features.
   */
  it should "align runtime edge policy through retiming and runtime log pipeline stages" in {
    val params = paramsFor(
      fixedType,
      runtimeLogMode    = true,
      runtimeEdgePolicy = true,
      retiming          = true,
      addPipeStages     = 1,
      mulPipeStages     = 1
    )
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode = CFARMode.SmallestOf,
        thresholdScale = 1.0,
        logMode        = true,
        referenceCells = edgeReferenceCells,
        guardCells     = edgeGuardCells,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame
      )
      expectFrame(
        dut,
        fixedInput,
        Some(expectedFrame(
          params,
          fixedInput,
          CFARModel.SOCA,
          thresholdScale = 1.0,
          logMode        = true,
          referenceCells = edgeReferenceCells,
          guardCells     = edgeGuardCells,
          edgePolicy     = CFAREdgePolicy.WrapAroundFrame
        )),
        readyPattern = Seq(true, false, true, true, false, true)
      )
    }
  }

  /**
   * Sends two different frames through WrapAroundFrame and proves circular references come only from the active frame, not from a neighboring chirp.
   */
  it should "keep wraparound windows inside each frame" in {
    val params      = paramsFor(fixedType, edgePolicy = CFAREdgePolicy.WrapAroundFrame)
    val firstFrame  = (0 until fftSize).map(_.toDouble)
    val secondFrame = (0 until fftSize).map(i => (100 + i).toDouble)
    test(new CFAR(params)).withAnnotations(annotations) { dut =>
      configure(
        dut,
        cfarMode       = CFARMode.CellAveraging,
        referenceCells = edgeReferenceCells,
        guardCells     = edgeGuardCells
      )
      expectFrame(
        dut,
        firstFrame,
        Some(expectedFrame(
          params,
          firstFrame,
          CFARModel.ClassicalCA,
          referenceCells = edgeReferenceCells,
          guardCells     = edgeGuardCells,
          edgePolicy     = CFAREdgePolicy.WrapAroundFrame
        ))
      )
      expectFrame(
        dut,
        secondFrame,
        Some(expectedFrame(
          params,
          secondFrame,
          CFARModel.ClassicalCA,
          referenceCells = edgeReferenceCells,
          guardCells     = edgeGuardCells,
          edgePolicy     = CFAREdgePolicy.WrapAroundFrame
        ))
      )
    }
  }

  /**
   * Rejects unsupported DSP data types before elaboration can silently build an invalid CFAR configuration.
   */
  it should "reject unsupported hardware data types" in {
    assertThrows[IllegalArgumentException] {
      CFARTypeSupport.requireSupportedType(new DspReal, "inputType")
    }
  }
}
