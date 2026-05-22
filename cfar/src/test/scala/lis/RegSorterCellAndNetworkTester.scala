package opera.lis

import chisel3._
import chiseltest._
import dsptools.numbers.Real
import opera.cfar.CFARTestConfig

trait RegSorterCellAndNetworkTester { this: ChiselScalatestTester =>
  import RegSorterModels._
  import SorterCellScenarios.RegSorterScenario

  protected def checkRegSorterScenarioMatrix[T <: Data: Real](
    dataType : T,
    label    : String,
    scenarios: Seq[RegSorterScenario]
  ): Unit = {
    require(scenarios.nonEmpty, "RegSorterCell matrix must contain at least one scenario")
    val size = scenarios.head.sortedData.length - 1
    require(size > 0, "RegSorterCell matrix needs at least one data cell plus sentinel")
    require(scenarios.forall(_.sortedData.length == size + 1), "All RegSorterCell matrix scenarios must use the same vector size")

    val params = LISParams[T](
      dataType = dataType,
      maxWindowSize = size,
      sorterType    = LISType.RegBased
    )

    (1 to size + 1).foreach { index =>
      test(new RegSorterCell(params, index)).withAnnotations(CFARTestConfig.annotations) { dut =>
        scenarios.foreach { scenario =>
          val trace = RegSorterNetworkModel(
            scenario.sortedData,
            scenario.removeData,
            scenario.insertData
          )
          val cell = trace.cell(index)

          if (CFARTestConfig.verbose) {
            println(
              s"RegSorterCell $label/${scenario.label} index=$index sorted=${scenario.sortedData.mkString("[", ", ", "]")} " +
                s"rm=${scenario.removeData} insert=${scenario.insertData} next=${trace.nextSortedData.mkString("[", ", ", "]")}"
            )
          }

          dut.io.i_prev_kept_before_insert.poke(cell.previousKeptBeforeInsert.B)
          dut.io.i_data_insert.poke(LISStreamingSorterTester.literalFor(scenario.insertData, dataType))
          dut.io.i_current_sorted_data.poke(LISStreamingSorterTester.literalFor(scenario.sortedData(index - 1), dataType))
          dut.io.i_next_sorted_data.foreach(_.poke(LISStreamingSorterTester.literalFor(scenario.sortedData(index), dataType)))

          dut.io.i_data_remove.foreach(_.poke(LISStreamingSorterTester.literalFor(scenario.removeData, dataType)))
          dut.io.i_previous_next_data.foreach(_.poke(LISStreamingSorterTester.literalFor(cell.previousNextData.get, dataType)))

          dut.io.o_data.expect(LISStreamingSorterTester.literalFor(cell.data, dataType))
          dut.io.o_kept_before_insert.expect(cell.keptBeforeInsert.B)
          dut.io.o_next_data.foreach(_.expect(LISStreamingSorterTester.literalFor(cell.nextData.get, dataType)))
          dut.clock.step()
        }
      }
    }
  }

  protected def checkRegSorterNetworkScenarioMatrix[T <: Data: Real](
    dataType : T,
    label    : String,
    scenarios: Seq[RegSorterScenario]
  ): Unit = {
    require(scenarios.nonEmpty, "RegSorterNetwork matrix must contain at least one scenario")
    val size = scenarios.head.sortedData.length - 1
    require(scenarios.forall(_.sortedData.length == size + 1), "All RegSorterNetwork scenarios must use the same vector size")

    val params = LISParams[T](
      dataType      = dataType,
      maxWindowSize = size,
      sorterType    = LISType.RegBased
    )

    test(new RegSorterNetwork(params)).withAnnotations(CFARTestConfig.annotations) { dut =>
      scenarios.foreach { scenario =>
        val trace = RegSorterNetworkModel(
          scenario.sortedData,
          scenario.removeData,
          scenario.insertData
        )

        dut.io.i_data_remove.poke(LISStreamingSorterTester.literalFor(scenario.removeData, dataType))
        dut.io.i_data_insert.poke(LISStreamingSorterTester.literalFor(scenario.insertData, dataType))
        scenario.sortedData.zipWithIndex.foreach { case (value, index) =>
          dut.io.i_sorted_data(index).poke(LISStreamingSorterTester.literalFor(value, dataType))
        }
        trace.nextSortedData.zipWithIndex.foreach { case (value, index) =>
          dut.io.o_next_sorted_data(index).expect(LISStreamingSorterTester.literalFor(value, dataType))
        }

        if (CFARTestConfig.verbose) {
          println(s"RegSorterNetwork $label/${scenario.label} next=${trace.nextSortedData.mkString("[", ", ", "]")}")
        }
        dut.clock.step()
      }
    }
  }
}
