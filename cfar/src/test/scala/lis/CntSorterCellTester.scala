package opera.lis

import chisel3._
import chiseltest._
import dsptools.numbers.Real
import opera.cfar.TestConfig

trait CntSorterCellTester { this: ChiselScalatestTester =>
  import CntSorterCellModels._

  protected def checkCntSorterCellCtrlTruthTable(): Unit = {
    test(new CntSorterCellCtrl).withAnnotations(TestConfig.annotations) { dut =>
      for {
        currentLessThanInput <- Seq(false, true)
        leftLessThanInput    <- Seq(false, true)
        rightLessThanInput   <- Seq(false, true)
        removeCurrent        <- Seq(false, true)
        discardFromRight     <- Seq(false, true)
      } {
        val expected = CntSorterCellCtrlModel(
          currentLessThanInput = currentLessThanInput,
          leftLessThanInput    = leftLessThanInput,
          rightLessThanInput   = rightLessThanInput,
          removeCurrent        = removeCurrent,
          discardFromRight     = discardFromRight
        )

        dut.io.i_current_less_than_input.poke(currentLessThanInput.B)
        dut.io.i_left_less_than_input.poke(leftLessThanInput.B)
        dut.io.i_right_less_than_input.poke(rightLessThanInput.B)
        dut.io.i_remove_current.poke(removeCurrent.B)
        dut.io.i_discard_from_right.poke(discardFromRight.B)

        dut.io.o_discard_to_left.expect(expected.discardToLeft.B)
        dut.io.o_update_cell.expect(expected.updateCell.B)
        dut.io.o_shift_from_right.expect(expected.shiftFromRight.B)
        dut.io.o_reset_fifo_position.expect(expected.resetFifoPosition.B)

        if (TestConfig.verbose) {
          println(
            s"counter sorter cellCtrl curr=$currentLessThanInput left=$leftLessThanInput right=$rightLessThanInput " +
              s"remove=$removeCurrent discardFromRight=$discardFromRight expected=$expected"
          )
        }
        dut.clock.step()
      }
    }
  }

  protected def checkCntSorterCellScenario[T <: Data: Real](
    params: LISParams[T],
    index : Int,
    steps : Seq[(String, CntSorterCellStepInput)]
  ): Unit = {
    val model = new CntSorterCellModel(
      dataType   = params.dataType,
      sorterSize = params.maxWindowSize,
      index      = index
    )

    test(new CntSorterCell(params, index)).withAnnotations(TestConfig.annotations) { dut =>
      val prime = CntSorterCellStepInput(
        enableSort       = false,
        state            = 0,
        leftCell         = CntSorterCellValue(0.0),
        rightCell        = CntSorterCellValue(0.0),
        data             = 0.0,
        discardFromRight = false,
        windowSize       = params.maxWindowSize,
        active           = true,
        lastCell         = index == params.maxWindowSize - 1
      )
      pokeCntSorterCellInput(dut, prime)
      model.step(prime)
      dut.clock.step()

      steps.foreach { case (label, input) =>
        val expected = model.step(input)
        pokeCntSorterCellInput(dut, input)
        expectCntSorterCellOutput(dut, expected, index, label)
        dut.clock.step()
      }
    }
  }

  private def pokeCntSorterCellState[T <: Data](
    signal: CntSorterCellState[T],
    value : CntSorterCellValue,
    dataType: T
  ): Unit = {
    signal.sorted_data.poke(LISStreamingSorterTester.literalFor(value.sortedData, dataType))
    signal.fifo_position.poke(value.fifoPosition.U)
    signal.is_less_than_input.poke(value.isLessThanInput.B)
  }

  private def pokeCntSorterCellInput[T <: Data: Real](
    dut  : CntSorterCell[T],
    input: CntSorterCellStepInput
  ): Unit = {
    dut.io.i_enable_sort.poke(input.enableSort.B)
    dut.io.i_state.poke(input.state.U)
    pokeCntSorterCellState(dut.io.i_left_cell, input.leftCell, dut.params.dataType)
    pokeCntSorterCellState(dut.io.i_right_cell, input.rightCell, dut.params.dataType)
    dut.io.i_data.poke(LISStreamingSorterTester.literalFor(input.data, dut.params.dataType))
    dut.io.i_discard_from_right.poke(input.discardFromRight.B)
    dut.io.i_window_size.foreach(_.poke(input.windowSize.U))
    dut.io.i_active.foreach(_.poke(input.active.B))
    dut.io.i_last_cell.foreach(_.poke(input.lastCell.B))
  }

  private def expectCntSorterCellOutput[T <: Data: Real](
    dut     : CntSorterCell[T],
    expected: CntSorterCellStepResult,
    index   : Int,
    label   : String
  ): Unit = {
    dut.io.o_cell_state.sorted_data.expect(LISStreamingSorterTester.literalFor(expected.cellState.sortedData, dut.params.dataType))
    dut.io.o_cell_state.is_less_than_input.expect(expected.cellState.isLessThanInput.B)
    dut.io.o_cell_state.fifo_position.expect(expected.cellState.fifoPosition.U)
    expected.dataToLeft.foreach(value => dut.io.o_data_to_left.expect(LISStreamingSorterTester.literalFor(value, dut.params.dataType)))
    dut.io.o_data_to_right.expect(LISStreamingSorterTester.literalFor(expected.dataToRight, dut.params.dataType))
    dut.io.o_remove_current.expect(expected.removeCurrent.B)
    if (index != 0) {
      dut.io.o_discard_to_left.expect(expected.discardToLeft.get.B)
    }

    if (TestConfig.verbose) {
      println(
        s"CntSorterCell $label expected cell=${expected.cellState} rightOut=${expected.dataToRight} " +
          s"remove=${expected.removeCurrent} update=${expected.updateCell} " +
          s"shiftFromRight=${expected.shiftFromRight} resetFifoPosition=${expected.resetFifoPosition}"
      )
    }
  }
}
