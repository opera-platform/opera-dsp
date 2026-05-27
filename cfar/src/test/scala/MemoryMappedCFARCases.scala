package opera.cfar

import opera.lis.LISType

private[cfar] final case class MemoryMappedCFARRuntimeConfig(
  mode          : CFARModel.CAMode,
  thresholdScale: Double,
  logMode       : Boolean = false,
  referenceCells: Int,
  guardCells    : Int,
  peakGrouping  : Boolean = false,
  edgePolicy    : Int,
  orderRankLeft : Int = 1,
  orderRankRight: Int = 1
)

private[cfar] final case class MemoryMappedCFARFrameCase(
  name                       : String,
  fftSize                    : Int,
  config                     : MemoryMappedCFARRuntimeConfig,
  dataSeed                   : Long,
  readyPattern               : Seq[Boolean] = Seq(true),
  randomReadyValidSeed       : Option[Long] = None,
  defaultRandomReadyValidSeed: Long = 0L,
  randomReadyValid           : Boolean = true,
  inputData                  : Option[Seq[Double]] = None,
  plotName                   : Option[String] = None
)

private[cfar] final case class MemoryMappedCFARCase(
  family           : String,
  maxFftSize       : Int,
  maxReferenceCells: Int,
  maxGuardCells    : Int,
  cfarType         : Int = CFARType.CellAveraging,
  lisType          : String = LISType.CntBased,
  sendCut          : Boolean = true,
  logMode          : Boolean = false,
  runtimeLogMode   : Boolean = false,
  edgePolicy       : Int = CFAREdgePolicy.OneSidedAverage,
  runtimeEdgePolicy: Boolean = false,
  retiming         : Boolean = false,
  addPipeStages    : Int = 0,
  mulPipeStages    : Int = 0
)

private[cfar] sealed trait MemoryMappedCFARCheck
private[cfar] final case class SingleFrameSweepCheck(frames: Seq[MemoryMappedCFARFrameCase]) extends MemoryMappedCFARCheck
private[cfar] final case class TwoFrameReconfigCheck(first: MemoryMappedCFARFrameCase, second: MemoryMappedCFARFrameCase) extends MemoryMappedCFARCheck
private[cfar] final case class MidFramePendingConfigCheck(
  first  : MemoryMappedCFARFrameCase,
  pending: MemoryMappedCFARFrameCase,
  second : MemoryMappedCFARFrameCase,
  updateAfterAcceptedIndex: Int
) extends MemoryMappedCFARCheck
