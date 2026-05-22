package opera.lis

import CntSorterCellModels.{CntSorterCellStepInput, CntSorterCellValue}

object SorterCellScenarios {
  final case class RegSorterScenario(
    label     : String,
    sortedData: Seq[Double],
    removeData: Double,
    insertData: Double
  )

  private def cntSorterCellStep(
    label           : String,
    sample          : Double,
    left            : CntSorterCellValue,
    right           : CntSorterCellValue,
    discardFromRight: Boolean = false,
    windowSize      : Int = 4,
    active          : Boolean = true,
    lastCell        : Boolean = false,
    enableSort      : Boolean = true,
    state           : Int = 1
  ): (String, CntSorterCellStepInput) =
    label -> CntSorterCellStepInput(
      enableSort       = enableSort,
      state            = state,
      data             = sample,
      leftCell         = left,
      rightCell        = right,
      discardFromRight = discardFromRight,
      windowSize       = windowSize,
      active           = active,
      lastCell         = lastCell
    )

  val uintRegSorterCellMovement: Seq[RegSorterScenario] = Seq(
    RegSorterScenario("remove first insert before all"        , Seq(2.0, 4.0, 6.0, 8.0, 99.0), 2.0, 1.0),
    RegSorterScenario("remove middle insert middle"           , Seq(1.0, 3.0, 5.0, 7.0, 99.0), 5.0, 4.0),
    RegSorterScenario("remove last insert after active values", Seq(1.0, 3.0, 5.0, 7.0, 99.0), 7.0, 9.0),
    RegSorterScenario("duplicate first-match replacement"     , Seq(1.0, 3.0, 3.0, 7.0, 99.0), 3.0, 3.0)
  )

  val signedRegSorterCellMovement: Seq[RegSorterScenario] = Seq(
    RegSorterScenario("remove first negative insert smaller", Seq(-8.0, -4.0, 0.0, 6.0, 50.0), -8.0, -10.0),
    RegSorterScenario("remove last active insert middle"    , Seq(-8.0, -4.0, 0.0, 6.0, 50.0), 6.0, 4.0),
    RegSorterScenario("duplicate signed insert"             , Seq(-8.0, -4.0, -4.0, 6.0, 50.0), -4.0, -4.0)
  )

  val fixedPointRegSorterCellMovement: Seq[RegSorterScenario] = Seq(
    RegSorterScenario("fractional remove middle insert middle"   , Seq(-2.0, -0.5, 1.25, 3.75, 7.5), 1.25, 0.5),
    RegSorterScenario("fractional remove first insert before all", Seq(-2.0, -0.5, 1.25, 3.75, 7.5), -2.0, -3.0),
    RegSorterScenario("fractional duplicate insert"              , Seq(-2.0, -0.5, -0.5, 3.75, 7.5), -0.5, -0.5)
  )

  val uintRegSorterNetworkMovement: Seq[RegSorterScenario] = Seq(
    RegSorterScenario("remove first insert middle"            , Seq(2.0, 4.0, 6.0, 8.0, 99.0), 2.0, 5.0),
    RegSorterScenario("remove duplicate insert before all"    , Seq(1.0, 3.0, 3.0, 7.0, 99.0), 3.0, 0.0),
    RegSorterScenario("remove last insert after active values", Seq(1.0, 3.0, 5.0, 7.0, 99.0), 7.0, 10.0)
  )

  val cntSorterFifoIndexZeroMovement: Seq[(String, CntSorterCellStepInput)] = Seq(
    cntSorterCellStep("index zero accepts right neighbor with window size 1",   5.0, CntSorterCellValue(0.0), CntSorterCellValue(12.0), windowSize = 1),
    cntSorterCellStep("index zero holds register while enable sort is low"  , 200.0, CntSorterCellValue(0.0), CntSorterCellValue(100.0, fifoPosition = 1), windowSize = 2, enableSort = false),
    cntSorterCellStep("index zero loads right neighbor after hold"          ,  50.0, CntSorterCellValue(0.0), CntSorterCellValue(20.0, fifoPosition = 1), windowSize = 2)
  )

  val cntSorterFifoMiddleMovement: Seq[(String, CntSorterCellStepInput)] = Seq(
    cntSorterCellStep("middle cell loads from right on insertion boundary"            ,  5.0, CntSorterCellValue(2.0, isLessThanInput = true), CntSorterCellValue(8.0, fifoPosition = 2), windowSize = 4),
    cntSorterCellStep("middle cell loads from left when discard propagates from right",  5.0, CntSorterCellValue(4.0, fifoPosition = 1, isLessThanInput = true), CntSorterCellValue(9.0, fifoPosition = 2), discardFromRight = true, windowSize = 3),
    cntSorterCellStep("middle cell holds while enable sort is low"                    , 10.0, CntSorterCellValue(1.0, isLessThanInput = true), CntSorterCellValue(12.0, fifoPosition = 2), windowSize = 3, enableSort = false),
    cntSorterCellStep("middle cell inactive path loads reset value"                   ,  7.0, CntSorterCellValue(6.0, fifoPosition = 1, isLessThanInput = true), CntSorterCellValue(9.0, fifoPosition = 2), windowSize = 2, active = false),
    cntSorterCellStep("middle cell loads again during flush state"                    ,  3.0, CntSorterCellValue(2.0, isLessThanInput = true), CntSorterCellValue(11.0, fifoPosition = 1), windowSize = 4, state = 2),
    cntSorterCellStep("middle cell resets registered data in idle state"              ,  9.0, CntSorterCellValue(1.0, isLessThanInput = true), CntSorterCellValue(13.0, fifoPosition = 1), windowSize = 4, state = 0)
  )

  val cntSorterFifoLastMovement: Seq[(String, CntSorterCellStepInput)] = Seq(
    cntSorterCellStep("last active cell takes left neighbor and asserts discard"          , 5.0, CntSorterCellValue(6.0, fifoPosition = 2, isLessThanInput = true), CntSorterCellValue(0.0), windowSize = 4, lastCell = true),
    cntSorterCellStep("last cell no-load cycle advances FIFO position counter with size 3", 1.0, CntSorterCellValue(4.0, fifoPosition = 1), CntSorterCellValue(0.0), windowSize = 3, lastCell = true),
    cntSorterCellStep("last cell second no-load cycle reaches FIFO discard point"         , 1.0, CntSorterCellValue(4.0, fifoPosition = 1), CntSorterCellValue(0.0), windowSize = 3, lastCell = true)
  )

  val cntSorterSignedMovement: Seq[(String, CntSorterCellStepInput)] = Seq(
    cntSorterCellStep("SInt negative right-shift load"             , -1.0, CntSorterCellValue(-3.0, isLessThanInput = true), CntSorterCellValue(4.0, fifoPosition = 2)),
    cntSorterCellStep("SInt left-shift load via propagated discard", -6.0, CntSorterCellValue(-8.0, isLessThanInput = true), CntSorterCellValue(2.0, fifoPosition = 2), discardFromRight = true)
  )

  val cntSorterFixedPointMovement: Seq[(String, CntSorterCellStepInput)] = Seq(
    cntSorterCellStep("FixedPoint fractional right-shift load"                      ,  0.5, CntSorterCellValue(-1.25, isLessThanInput = true), CntSorterCellValue(2.5, fifoPosition = 2)),
    cntSorterCellStep("FixedPoint fractional left-shift load via propagated discard", 0.75, CntSorterCellValue(-0.25, fifoPosition = 1, isLessThanInput = true), CntSorterCellValue(3.0, fifoPosition = 2), discardFromRight = true)
  )
}
