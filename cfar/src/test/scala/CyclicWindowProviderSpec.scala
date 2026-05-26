package opera.cfar

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CyclicWindowProviderSpec extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of "CyclicWindowProvider"

  override protected def annotations = splitOutputAnnotations

  private def checkReplay(size: Int, referenceCells: Int, guardCells: Int, maxReferenceCells: Int, maxGuardCells: Int): Unit = {
    val params = CyclicWindowProviderTester.paramsForReplay(size, maxReferenceCells, maxGuardCells)

    test(new CyclicWindowProvider(params)).withAnnotations(annotations) { dut =>
      CyclicWindowProviderTester.driveReplayAndCheck(dut, size, referenceCells, guardCells)
    }
  }

  it should "emit cyclic windows for a 256-bin frame with a wide reference span" in {
    checkReplay(size = 256, referenceCells = 32, guardCells = 3, maxReferenceCells = 32, maxGuardCells = 4)
  }

  it should "emit cyclic windows for a 1024-bin frame with a longer replay sequence" in {
    checkReplay(size = 1024, referenceCells = 16, guardCells = 4, maxReferenceCells = 16, maxGuardCells = 4)
  }
}
