package opera.lis

import chisel3._
import chiseltest._
import fixedpoint._
import opera.cfar.TestConfigSupport
import org.scalatest.flatspec.AnyFlatSpec

class RegSorterCellAndNetworkSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with TestConfigSupport
    with RegSorterCellAndNetworkTester {
  behavior of "register sorter cells and network"

  it should "check RegSorterCell UInt cells across remove and insert positions" in {
    checkRegSorterScenarioMatrix(
      dataType  = UInt(8.W),
      label     = "UInt RegSorterCell movement matrix",
      scenarios = SorterCellScenarios.uintRegSorterCellMovement
    )
  }

  it should "check RegSorterCell signed and fixed-point cells across value ranges" in {
    checkRegSorterScenarioMatrix(
      dataType  = SInt(8.W),
      label     = "SInt RegSorterCell movement matrix",
      scenarios = SorterCellScenarios.signedRegSorterCellMovement
    )

    checkRegSorterScenarioMatrix(
      dataType  = FixedPoint(12.W, 4.BP),
      label     = "FixedPoint RegSorterCell movement matrix",
      scenarios = SorterCellScenarios.fixedPointRegSorterCellMovement
    )
  }

  it should "check RegSorterNetwork against the RegSorterCell network model" in {
    checkRegSorterNetworkScenarioMatrix(
      dataType  = UInt(8.W),
      label     = "UInt register sorter network",
      scenarios = SorterCellScenarios.uintRegSorterNetworkMovement
    )
  }
}
