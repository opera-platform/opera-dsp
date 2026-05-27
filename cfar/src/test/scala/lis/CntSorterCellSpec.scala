package opera.lis

import chisel3._
import chiseltest._
import fixedpoint._
import opera.cfar.TestConfigSupport
import org.scalatest.flatspec.AnyFlatSpec

class CntSorterCellSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with TestConfigSupport
    with CntSorterCellTester {
  behavior of "counter sorter cells"

  it should "check CntSorterCell ctrl logic for every ctrl-input combination" in {
    checkCntSorterCellCtrlTruthTable()
  }

  it should "move CntSorterCell FIFO data through index zero, middle, and last cells" in {
    val fifoParams = LISParams[UInt](
      dataType      = UInt(8.W),
      maxWindowSize = 4,
      sorterType    = LISType.CntBased,
      runTime       = true
    )

    checkCntSorterCellScenario(
      params = fifoParams,
      index  = 0,
      steps  = SorterCellScenarios.cntSorterFifoIndexZeroMovement
    )
    checkCntSorterCellScenario(
      params = fifoParams,
      index  = 1,
      steps  = SorterCellScenarios.cntSorterFifoMiddleMovement
    )
    checkCntSorterCellScenario(
      params = fifoParams,
      index  = 3,
      steps  = SorterCellScenarios.cntSorterFifoLastMovement
    )
  }

  it should "check CntSorterCell signed and fixed-point multi-cycle movement" in {
    checkCntSorterCellScenario(
      params = LISParams[SInt](
        dataType      = SInt(8.W),
        maxWindowSize = 4,
        sorterType    = LISType.CntBased,
        runTime       = true
      ),
      index = 1,
      steps = SorterCellScenarios.cntSorterSignedMovement
    )

    checkCntSorterCellScenario(
      params = LISParams[FixedPoint](
        dataType      = FixedPoint(12.W, 4.BP),
        maxWindowSize = 4,
        sorterType    = LISType.CntBased,
        runTime       = true
      ),
      index = 1,
      steps = SorterCellScenarios.cntSorterFixedPointMovement
    )
  }
}
