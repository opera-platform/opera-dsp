package opera.cfar

import chisel3._
import chiseltest.ChiselScalatestTester
import dsptools.numbers._
import opera.lis.LISType
import org.scalatest.TestSuite

private[cfar] final case class CFARTestWindowCase(fftSize: Int, referenceCells: Int, guardCells: Int) {
  val edgeSpan: Int = referenceCells + guardCells

  require(fftSize > 2 * referenceCells + 2 * guardCells + 1, s"Illegal CFAR window: $this")
}

trait TestUtils extends TestConfigSupport { this: ChiselScalatestTester with TestSuite =>
  protected def annotations = TestConfig.annotations
  protected val defaultFftSize = 32
  protected val allEdgePolicies: Seq[Int] = Seq(
    CFAREdgePolicy.SuppressEdges,
    CFAREdgePolicy.OneSidedAverage,
    CFAREdgePolicy.WrapAroundFrame
  )

  protected def edgePolicyName(edgePolicy: Int): String = edgePolicy match {
    case CFAREdgePolicy.SuppressEdges   => "SuppressEdges"
    case CFAREdgePolicy.OneSidedAverage => "OneSidedAverage"
    case CFAREdgePolicy.WrapAroundFrame => "WrapAroundFrame"
    case other                          => s"Unknown($other)"
  }

  /** Creates a compact CFAR configuration for unit tests. */
  protected def paramsFor[T <: Data: Real](
    dataType         : T,
    maxFftSize       : Int = defaultFftSize,
    maxReferenceCells: Int = 8,
    maxGuardCells    : Int = 2,
    cfarType         : Int = CFARType.CellAveraging,
    lisType          : String = LISType.CntBased,
    sendCut          : Boolean = true,
    logMode          : Boolean = false,
    runtimeLogMode   : Boolean = false,
    edgePolicy       : Int = CFAREdgePolicy.OneSidedAverage,
    runtimeEdgePolicy: Boolean = false,
    retiming         : Boolean = false,
    addPipeStages    : Int = 0,
    mulPipeStages    : Int = 0,
    minSRAMDepth     : Int = 8
  ): CFARParams[T] = {
    CFARParams(
      inputType         = dataType,
      thresholdType     = dataType,
      scaleType         = dataType,
      cfarType          = cfarType,
      lisType           = lisType,
      maxReferenceCells = maxReferenceCells,
      maxGuardCells     = maxGuardCells,
      maxFftSize        = maxFftSize,
      sendCut           = sendCut,
      logMode           = logMode,
      runtimeLogMode    = runtimeLogMode,
      edgePolicy        = edgePolicy,
      runtimeEdgePolicy = runtimeEdgePolicy,
      retiming          = retiming,
      addPipeStages     = addPipeStages,
      mulPipeStages     = mulPipeStages,
      minSRAMDepth      = minSRAMDepth
    )
  }

  /** Creates a compact CFAR configuration for a specific test window. */
  protected def paramsForWindow[T <: Data: Real](
    dataType         : T,
    window           : CFARTestWindowCase,
    maxReferenceCells: Int = 8,
    maxGuardCells    : Int = 2,
    cfarType         : Int = CFARType.CellAveraging,
    lisType          : String = LISType.CntBased,
    sendCut          : Boolean = true,
    logMode          : Boolean = false,
    runtimeLogMode   : Boolean = false,
    edgePolicy       : Int = CFAREdgePolicy.OneSidedAverage,
    runtimeEdgePolicy: Boolean = false,
    retiming         : Boolean = false,
    addPipeStages    : Int = 0,
    mulPipeStages    : Int = 0,
    minSRAMDepth     : Int = 8
  ): CFARParams[T] = {
    paramsFor(
      dataType,
      maxFftSize = window.fftSize,
      maxReferenceCells = maxReferenceCells,
      maxGuardCells = maxGuardCells,
      cfarType = cfarType,
      lisType = lisType,
      sendCut = sendCut,
      logMode = logMode,
      runtimeLogMode = runtimeLogMode,
      edgePolicy = edgePolicy,
      runtimeEdgePolicy = runtimeEdgePolicy,
      retiming = retiming,
      addPipeStages = addPipeStages,
      mulPipeStages = mulPipeStages,
      minSRAMDepth = minSRAMDepth
    )
  }
}
