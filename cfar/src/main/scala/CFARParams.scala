package opera.cfar

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util.isPow2
import dsptools.numbers.Real
import fixedpoint._
import opera.lis.LISType

object CFARMode {
  val CellAveraging: Int = 0
  val GreatestOf: Int = 1
  val SmallestOf: Int = 2
}

object CFARType {
  val CellAveraging   : Int = 0 // CA / GOCA / SOCA family
  val OrderedStatistic: Int = 1 // OS / GOS family

  def isValid(cfarType: Int): Boolean = cfarType == CellAveraging || cfarType == OrderedStatistic
}

object CFAREdgePolicy {
  val SuppressEdges  : Int = 0
  val OneSidedAverage: Int = 1
  val WrapAroundFrame: Int = 2

  def isValid(policy: Int): Boolean = policy >= SuppressEdges && policy <= WrapAroundFrame
}

object CFARTypeSupport {
  def requireSupportedParams[T <: Data: Real](params: CFARParams[T]): Unit = {
    requireSupportedType(params.inputType    , "inputType")
    requireSupportedType(params.thresholdType, "thresholdType")
    requireSupportedType(params.scaleType    , "scaleType")
  }

  def requireSupportedType(data: Data, name: String): Unit = {
    require(
      isSupportedType(data),
      s"$name must be FixedPoint, UInt, or SInt. CFAR hardware does not support ${data.getClass.getSimpleName}."
    )
  }

  private def isSupportedType(data: Data): Boolean = data match {
    case _: FixedPoint => true
    case _: UInt       => true
    case _: SInt       => true
    case _             => false
  }
}

case class CFARParams[T <: Data: Real](
  inputType        : T,
  thresholdType    : T,
  scaleType        : T,
  cfarType         : Int = CFARType.CellAveraging,
  lisType          : String = LISType.CntBased,
  maxReferenceCells: Int = 16,
  maxGuardCells    : Int = 4,
  maxFftSize       : Int = 1024,
  sendCut          : Boolean = true,
  logMode          : Boolean = false,
  runtimeLogMode   : Boolean = false,
  edgePolicy       : Int = CFAREdgePolicy.OneSidedAverage,
  runtimeEdgePolicy: Boolean = false,
  retiming         : Boolean = false,
  addPipeStages    : Int = 0,
  mulPipeStages    : Int = 0,
  minSRAMDepth     : Int = 8
) {
  private val isOrderedStatistic = cfarType == CFARType.OrderedStatistic

  require(isPow2(maxReferenceCells), "maxReferenceCells must be a power of two")
  require(isPow2(maxFftSize)       , "maxFftSize must be a power of two")
  require(maxReferenceCells > 0    , "maxReferenceCells must be positive")
  require(maxGuardCells > 0        , "maxGuardCells must be positive")
  if (!isOrderedStatistic) {
    require(maxReferenceCells > maxGuardCells, "maxReferenceCells must be greater than maxGuardCells")
  }
  require(CFAREdgePolicy.isValid(edgePolicy), s"edgePolicy must be a supported CFAREdgePolicy value, got $edgePolicy")
  require(CFARType.isValid(cfarType)        , s"cfarType must be a supported CFARType value, got $cfarType")
  require(addPipeStages >= 0                , "addPipeStages must be non-negative")
  require(mulPipeStages >= 0                , "mulPipeStages must be non-negative")
  require(minSRAMDepth >= 0                 , "minSRAMDepth must be non-negative")

  // The ordered-statistic family selects ranked reference cells through a Linear Insertion Sorter.
  if (isOrderedStatistic) {
    require(LISType.all.contains(lisType), s"lisType must be one of ${LISType.all.mkString(", ")}")
  }

  requireIsChiselType(inputType)
  requireIsChiselType(thresholdType)
  requireIsChiselType(scaleType)
  CFARTypeSupport.requireSupportedParams(this)
}

object CFARParams {
  def fixed(
    inputType        : FixedPoint = FixedPoint(16.W, 8.BP),
    thresholdType    : FixedPoint = FixedPoint(16.W, 8.BP),
    scaleType        : FixedPoint = FixedPoint(16.W, 8.BP),
    cfarType         : Int = CFARType.CellAveraging,
    lisType          : String = LISType.CntBased,
    maxReferenceCells: Int = 16,
    maxGuardCells    : Int = 4,
    maxFftSize       : Int = 1024,
    sendCut          : Boolean = true,
    logMode          : Boolean = false,
    runtimeLogMode   : Boolean = false,
    edgePolicy       : Int = CFAREdgePolicy.OneSidedAverage,
    runtimeEdgePolicy: Boolean = false,
    retiming         : Boolean = false,
    addPipeStages    : Int = 0,
    mulPipeStages    : Int = 0,
    minSRAMDepth     : Int = 8
  ): CFARParams[FixedPoint] = {
    CFARParams(
      inputType         = inputType,
      thresholdType     = thresholdType,
      scaleType         = scaleType,
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
}
