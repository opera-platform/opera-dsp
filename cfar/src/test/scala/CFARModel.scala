package opera.cfar

import chisel3._
import chisel3.util.log2Ceil
import fixedpoint._

object CFARModel {
  final case class CAMode(name: String, value: Int)
  final case class RawValue(raw: BigInt)
  final case class ExpectedBin(cut: RawValue, threshold: RawValue, peak: Boolean, trace: BinTrace)
  final case class SideTrace(sum: BigInt, average: BigInt)
  final case class BinTrace(
    bin                 : Int,
    left                : Option[SideTrace],
    right               : Option[SideTrace],
    selectedAverage     : BigInt,
    hasLeft             : Boolean,
    hasRight            : Boolean,
    isEdge              : Boolean,
    edgePolicy          : String,
    edgeBehavior        : String,
    thresholdBeforeCast : BigInt,
    thresholdBinaryPoint: Int,
    thresholdAfterCast  : BigInt
  ) {
    override def toString: String = {
      val leftText = left.map(value => s"sum=${value.sum},avg=${value.average}").getOrElse("unavailable")
      val rightText = right.map(value => s"sum=${value.sum},avg=${value.average}").getOrElse("unavailable")
      s"bin=$bin left($leftText) right($rightText) " +
        s"selectedAverage=$selectedAverage hasLeft=$hasLeft hasRight=$hasRight isEdge=$isEdge " +
        s"edgePolicy=$edgePolicy edgeBehavior=$edgeBehavior " +
        s"thresholdBeforeCast=$thresholdBeforeCast thresholdBinaryPoint=$thresholdBinaryPoint " +
        s"thresholdAfterCast=$thresholdAfterCast"
    }
  }

  val ClassicalCA: CAMode = CAMode("Classical CA", CFARMode.CellAveraging)
  val GOCA    : CAMode = CAMode("GOCA", CFARMode.GreatestOf)
  val SOCA    : CAMode = CAMode("SOCA", CFARMode.SmallestOf)
  val caModes : Seq[CAMode] = Seq(ClassicalCA, GOCA, SOCA)
  val GOSCA   : CAMode = CAMode("GOS-CA", CFARMode.CellAveraging)
  val GOSGO   : CAMode = CAMode("GOS-GO", CFARMode.GreatestOf)
  val GOSSO   : CAMode = CAMode("GOS-SO", CFARMode.SmallestOf)
  val gosModes: Seq[CAMode] = Seq(GOSCA, GOSGO, GOSSO)

  private sealed trait NumericFormat {
    def width      : Int
    def binaryPoint: Int
    def signed     : Boolean

    protected val modulus: BigInt = BigInt(1) << width
    protected val signBit: BigInt = BigInt(1) << (width - 1)

    def wrap(raw: BigInt): BigInt = {
      val masked = raw & (modulus - 1)
      if (signed && masked >= signBit) masked - modulus else masked
    }

    def fromDouble(value: Double): BigInt
    def literal[T <: Data](raw: BigInt, dataType: T): T
  }

  private final case class FixedFormat(width: Int, binaryPoint: Int) extends NumericFormat {
    val signed: Boolean = true
    def fromDouble(value: Double): BigInt = wrap(BigInt(math.round(value * math.pow(2.0, binaryPoint.toDouble))))
    def literal[T <: Data](raw: BigInt, dataType: T): T = FixedPoint.fromBigInt(wrap(raw), dataType.getWidth.W, binaryPoint.BP).asInstanceOf[T]
  }

  private final case class UIntFormat(width: Int) extends NumericFormat {
    val binaryPoint: Int = 0
    val signed: Boolean = false
    def fromDouble(value: Double): BigInt = {
      require(value >= 0.0, s"UInt test literal must be non-negative, got $value")
      wrap(BigInt(math.round(value)))
    }
    def literal[T <: Data](raw: BigInt, dataType: T): T = wrap(raw).U(dataType.getWidth.W).asInstanceOf[T]
  }

  private final case class SIntFormat(width: Int) extends NumericFormat {
    val binaryPoint: Int = 0
    val signed: Boolean = true
    def fromDouble(value: Double): BigInt = wrap(BigInt(math.round(value)))
    def literal[T <: Data](raw: BigInt, dataType: T): T = wrap(raw).S(dataType.getWidth.W).asInstanceOf[T]
  }

  private def formatOf(dataType: Data): NumericFormat = dataType match {
    case fixed: FixedPoint =>
      require(fixed.widthKnown, "FixedPoint width must be known")
      require(fixed.binaryPoint.known, "FixedPoint binary point must be known")
      FixedFormat(fixed.getWidth, fixed.binaryPoint.get)
    case uint: UInt =>
      require(uint.widthKnown, "UInt width must be known")
      UIntFormat(uint.getWidth)
    case sint: SInt =>
      require(sint.widthKnown, "SInt width must be known")
      SIntFormat(sint.getWidth)
    case other =>
      throw new IllegalArgumentException(s"Unsupported CFAR model data type: ${other.getClass.getName}")
  }

  def literalFor[T <: Data](value: Double, dataType: T): T = {
    val format = formatOf(dataType)
    format.literal(format.fromDouble(value), dataType)
  }

  private def wrapIndex(index: Int, size: Int): Int = {
    val mod = index % size
    if (mod < 0) mod + size else mod
  }

  private def shiftRight(raw: BigInt, amount: Int, format: NumericFormat): BigInt =
    if (amount == 0) raw
    else if (format.signed) raw >> amount
    else raw / (BigInt(1) << amount)

  private def align(raw: BigInt, fromBinaryPoint: Int, toBinaryPoint: Int, signed: Boolean): BigInt = {
    val diff = toBinaryPoint - fromBinaryPoint
    if (diff == 0) raw
    else if (diff > 0) raw << diff
    else if (signed) raw >> -diff
    else raw / (BigInt(1) << -diff)
  }

  private def scaledThreshold(
      noise      : BigInt,
      inputFormat: NumericFormat,
      scale      : BigInt,
      scaleFormat: NumericFormat,
      logMode    : Boolean
  ): (BigInt, Int) =
    if (logMode) {
      val bp = inputFormat.binaryPoint.max(scaleFormat.binaryPoint)
      val noiseAligned = align(noise, inputFormat.binaryPoint, bp, inputFormat.signed)
      val scaleAligned = align(scale, scaleFormat.binaryPoint, bp, scaleFormat.signed)
      (noiseAligned + scaleAligned, bp)
    } else if (inputFormat.binaryPoint > 0 || scaleFormat.binaryPoint > 0) {
      val productBinaryPoint = inputFormat.binaryPoint + scaleFormat.binaryPoint
      val scaledBinaryPoint = inputFormat.binaryPoint.max(scaleFormat.binaryPoint) + 1
      (align(noise * scale, productBinaryPoint, scaledBinaryPoint, signed = true), scaledBinaryPoint)
    } else {
      (noise * scale, 0)
    }

  private def castToThreshold(raw: BigInt, binaryPoint: Int, thresholdFormat: NumericFormat): BigInt =
    thresholdFormat.wrap(align(raw, binaryPoint, thresholdFormat.binaryPoint, signed = true))

  private def compareCutToThreshold(
      cut                 : BigInt,
      threshold           : BigInt,
      thresholdBinaryPoint: Int,
      inputFormat         : NumericFormat
  ): Boolean = {
    val bp = inputFormat.binaryPoint.max(thresholdBinaryPoint)
    val cutAligned = align(cut, inputFormat.binaryPoint, bp, inputFormat.signed)
    val thresholdAligned = align(threshold, thresholdBinaryPoint, bp, signed = true)
    cutAligned > thresholdAligned
  }

  private def localMaximum(index: Int, samples: Seq[BigInt], edgePolicy: Int): Boolean = {
    val cut = samples(index)
    if (edgePolicy == CFAREdgePolicy.WrapAroundFrame) {
      cut > samples(wrapIndex(index - 1, samples.length)) && cut > samples(wrapIndex(index + 1, samples.length))
    } else {
      val prevOk = index == 0 || cut > samples(index - 1)
      val nextOk = index == samples.length - 1 || cut > samples(index + 1)
      prevOk && nextOk
    }
  }

  private def edgePolicyName(edgePolicy: Int): String = edgePolicy match {
    case CFAREdgePolicy.SuppressEdges   => "SuppressEdges"
    case CFAREdgePolicy.OneSidedAverage => "OneSidedAverage"
    case CFAREdgePolicy.WrapAroundFrame => "WrapAroundFrame"
    case other                          => s"Unknown($other)"
  }

  /**
   * Builds the model for CUT, threshold, and peak bits for one FFT frame.
   *
   * Per-side reference sums are shifted first, CA/GOCA/SOCA then chooses an average, 
   * and threshold scaling is applied before the peak comparison.
   */
  def expectedFrame[T <: Data](
    params        : CFARParams[T],
    data          : Seq[Double],
    cfarMode      : CAMode,
    thresholdScale: Double = 1.0,
    logMode       : Boolean = false,
    referenceCells: Int = 8,
    guardCells    : Int = 2,
    noiseDivShift : Int = -1,
    edgePolicy    : Int = CFAREdgePolicy.OneSidedAverage,
    peakGrouping  : Boolean = false
  ): Seq[ExpectedBin] = {
    require(data.nonEmpty, "CFAR model needs a non-empty FFT frame")
    require(referenceCells > 0, "referenceCells must be positive")
    require(guardCells > 0, "guardCells must be positive")

    val inputFormat     = formatOf(params.inputType)
    val scaleFormat     = formatOf(params.scaleType)
    val thresholdFormat = formatOf(params.thresholdType)
    val shift           = if (noiseDivShift >= 0) noiseDivShift else log2Ceil(referenceCells)
    val samples         = data.map(value => inputFormat.fromDouble(value))
    val scale           = scaleFormat.fromDouble(thresholdScale)
    val edgeSpan        = referenceCells + guardCells

    def sideTrace(index: Int, leftSide: Boolean, wrapFrame: Boolean): SideTrace = {
      val start =
        if (leftSide) index - edgeSpan
        else index + guardCells + 1
      val cells = (0 until referenceCells).map { offset =>
        val rawIndex = start + offset
        samples(if (wrapFrame) wrapIndex(rawIndex, data.length) else rawIndex)
      }
      val sum = cells.sum
      SideTrace(sum, shiftRight(sum, shift, inputFormat))
    }

    def modeAverage(left: SideTrace, right: SideTrace): BigInt = cfarMode.value match {
      case CFARMode.CellAveraging => shiftRight(left.average + right.average, 1, inputFormat)
      case CFARMode.GreatestOf    => if (left.average > right.average) left.average else right.average
      case CFARMode.SmallestOf    => if (left.average < right.average) left.average else right.average
      case other                  => throw new IllegalArgumentException(s"Unsupported CFAR mode: $other")
    }

    def averageForIndex(index: Int): (BigInt, Boolean, Option[SideTrace], Option[SideTrace], String, Boolean, Boolean, Boolean) = {
      val hasLeft   = index >= edgeSpan
      val hasRight  = index < data.length - edgeSpan
      val isEdge    = !hasLeft || !hasRight
      val wrapFrame = edgePolicy == CFAREdgePolicy.WrapAroundFrame
      val left      = if (wrapFrame || hasLeft) Some(sideTrace(index, leftSide = true, wrapFrame = wrapFrame)) else None
      val right     = if (wrapFrame || hasRight) Some(sideTrace(index, leftSide = false, wrapFrame = wrapFrame)) else None

      edgePolicy match {
        case CFAREdgePolicy.SuppressEdges =>
          if (isEdge) {
            (0, true, left, right, "suppress", hasLeft, hasRight, isEdge)
          } else {
            (modeAverage(left.get, right.get), false, left, right, "two-sided", hasLeft, hasRight, isEdge)
          }
        case CFAREdgePolicy.OneSidedAverage =>
          if (!hasLeft && hasRight) {
            (right.get.average, false, left, right, "right-only", hasLeft, hasRight, isEdge)
          } else if (hasLeft && !hasRight) {
            (left.get.average, false, left, right, "left-only", hasLeft, hasRight, isEdge)
          } else if (hasLeft && hasRight) {
            (modeAverage(left.get, right.get), false, left, right, "two-sided", hasLeft, hasRight, isEdge)
          } else {
            (0, false, left, right, "no-reference", hasLeft, hasRight, isEdge)
          }
        case CFAREdgePolicy.WrapAroundFrame =>
          (modeAverage(left.get, right.get), false, left, right, "wrap", hasLeft, hasRight, isEdge)
        case other =>
          throw new IllegalArgumentException(s"Unsupported CFAR edge policy: $other")
      }
    }

    data.indices.map { index =>
      val cut = samples(index)
      val (avg, suppressPeak, left, right, edgeBehavior, hasLeft, hasRight, isEdge) = averageForIndex(index)
      val (thresholdForCompare, thresholdBinaryPoint) = scaledThreshold(avg, inputFormat, scale, scaleFormat, logMode)
      val threshold = castToThreshold(thresholdForCompare, thresholdBinaryPoint, thresholdFormat)
      val aboveThreshold = compareCutToThreshold(cut, thresholdForCompare, thresholdBinaryPoint, inputFormat)
      val peak = !suppressPeak && aboveThreshold && (!peakGrouping || localMaximum(index, samples, edgePolicy))
      val trace = BinTrace(
        bin                  = index,
        left                 = left,
        right                = right,
        selectedAverage      = avg,
        hasLeft              = hasLeft,
        hasRight             = hasRight,
        isEdge               = isEdge,
        edgePolicy           = edgePolicyName(edgePolicy),
        edgeBehavior         = edgeBehavior,
        thresholdBeforeCast  = thresholdForCompare,
        thresholdBinaryPoint = thresholdBinaryPoint,
        thresholdAfterCast   = threshold
      )
      ExpectedBin(RawValue(cut), RawValue(threshold), peak, trace)
    }
  }

  def expectedGOSFrame[T <: Data](
    params        : CFARParams[T],
    data          : Seq[Double],
    cfarMode      : CAMode,
    thresholdScale: Double = 1.0,
    logMode       : Boolean = false,
    referenceCells: Int = 4,
    guardCells    : Int = 1,
    orderRankLeft : Int = 1,
    orderRankRight: Int = 1,
    edgePolicy    : Int = CFAREdgePolicy.SuppressEdges,
    peakGrouping  : Boolean = false
  ): Seq[ExpectedBin] = {
    require(data.nonEmpty, "GOS-CFAR model needs a non-empty FFT frame")
    require(referenceCells > 0, "referenceCells must be positive")
    require(guardCells > 0, "guardCells must be positive")
    require(orderRankLeft >= 1 && orderRankLeft <= referenceCells, "orderRankLeft must be inside reference window")
    require(orderRankRight >= 1 && orderRankRight <= referenceCells, "orderRankRight must be inside reference window")

    val inputFormat     = formatOf(params.inputType)
    val scaleFormat     = formatOf(params.scaleType)
    val thresholdFormat = formatOf(params.thresholdType)
    val samples         = data.map(value => inputFormat.fromDouble(value))
    val scale           = scaleFormat.fromDouble(thresholdScale)
    val edgeSpan        = referenceCells + guardCells

    def sideCells(index: Int, leftSide: Boolean, wrapFrame: Boolean): Seq[BigInt] = {
      val start =
        if (leftSide) index - edgeSpan
        else index + guardCells + 1
      (0 until referenceCells).map { offset =>
        val rawIndex = start + offset
        samples(if (wrapFrame) wrapIndex(rawIndex, data.length) else rawIndex)
      }
    }

    def selectedNoise(leftOrder: BigInt, rightOrder: BigInt): BigInt = cfarMode.value match {
      case CFARMode.CellAveraging => shiftRight(leftOrder + rightOrder, 1, inputFormat)
      case CFARMode.GreatestOf    => if (leftOrder > rightOrder) leftOrder else rightOrder
      case CFARMode.SmallestOf    => if (leftOrder < rightOrder) leftOrder else rightOrder
      case other                  => throw new IllegalArgumentException(s"Unsupported GOS-CFAR mode: $other")
    }

    data.indices.map { index =>
      val cut      = samples(index)
      val hasLeft  = index >= edgeSpan
      val hasRight = index < data.length - edgeSpan
      val isEdge   = !hasLeft || !hasRight

      val wrapFrame = edgePolicy == CFAREdgePolicy.WrapAroundFrame
      val left = if (wrapFrame || hasLeft) {
        val cells = sideCells(index, leftSide = true, wrapFrame = wrapFrame)
        val rankValue = cells.sorted.apply(orderRankLeft - 1)
        Some(SideTrace(cells.sum, rankValue))
      } else {
        None
      }
      val right = if (wrapFrame || hasRight) {
        val cells = sideCells(index, leftSide = false, wrapFrame = wrapFrame)
        val rankValue = cells.sorted.apply(orderRankRight - 1)
        Some(SideTrace(cells.sum, rankValue))
      } else {
        None
      }

      val (noise, suppressPeak, edgeBehavior) = edgePolicy match {
        case CFAREdgePolicy.SuppressEdges =>
          if (isEdge) (BigInt(0), true, "suppress")
          else (selectedNoise(left.get.average, right.get.average), false, "two-sided-gos")
        case CFAREdgePolicy.OneSidedAverage =>
          if (!hasLeft && hasRight) {
            (right.get.average, false, "right-only-gos")
          } else if (hasLeft && !hasRight) {
            (left.get.average, false, "left-only-gos")
          } else if (hasLeft && hasRight) {
            (selectedNoise(left.get.average, right.get.average), false, "two-sided-gos")
          } else {
            (BigInt(0), false, "no-reference")
          }
        case CFAREdgePolicy.WrapAroundFrame =>
          (selectedNoise(left.get.average, right.get.average), false, "wrap-gos")
        case other =>
          throw new IllegalArgumentException(s"Unsupported GOS-CFAR edge policy: $other")
      }
      val (thresholdForCompare, thresholdBinaryPoint) = scaledThreshold(noise, inputFormat, scale, scaleFormat, logMode)
      val threshold = if (suppressPeak) BigInt(0) else castToThreshold(thresholdForCompare, thresholdBinaryPoint, thresholdFormat)
      val aboveThreshold = compareCutToThreshold(cut, thresholdForCompare, thresholdBinaryPoint, inputFormat)
      val peak = !suppressPeak && aboveThreshold && (!peakGrouping || localMaximum(index, samples, edgePolicy))
      val trace = BinTrace(
        bin                  = index,
        left                 = left,
        right                = right,
        selectedAverage      = noise,
        hasLeft              = hasLeft,
        hasRight             = hasRight,
        isEdge               = isEdge,
        edgePolicy           = edgePolicyName(edgePolicy),
        edgeBehavior         = edgeBehavior,
        thresholdBeforeCast  = if (suppressPeak) 0 else thresholdForCompare,
        thresholdBinaryPoint = thresholdBinaryPoint,
        thresholdAfterCast   = threshold
      )
      ExpectedBin(RawValue(cut), RawValue(threshold), peak, trace)
    }
  }
}
