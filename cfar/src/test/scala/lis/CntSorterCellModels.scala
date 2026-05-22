package opera.lis

import chisel3._
import dsptools.numbers._
import fixedpoint._

object CntSorterCellModels {
  final case class CntSorterCellValue(
    sortedData     : Double,
    fifoPosition   : Int = 0,
    isLessThanInput: Boolean = false
  )

  final case class CntSorterCellStepInput(
    enableSort      : Boolean,
    state           : Int,
    leftCell        : CntSorterCellValue,
    rightCell       : CntSorterCellValue,
    data            : Double,
    discardFromRight: Boolean,
    windowSize      : Int,
    active          : Boolean,
    lastCell        : Boolean
  )

  final case class CntSorterCellStepResult(
    cellState        : CntSorterCellValue,
    dataToLeft       : Option[Double],
    dataToRight      : Double,
    removeCurrent    : Boolean,
    discardToLeft    : Option[Boolean],
    updateCell       : Boolean,
    shiftFromRight   : Boolean,
    resetFifoPosition: Boolean,
    selectedCellData : Double
  )

  final case class CntSorterCellCtrlResult(
    discardToLeft    : Boolean,
    updateCell       : Boolean,
    shiftFromRight   : Boolean,
    resetFifoPosition: Boolean
  )

  object CntSorterCellCtrlModel {
    def apply(
      currentLessThanInput: Boolean,
      leftLessThanInput   : Boolean,
      rightLessThanInput  : Boolean,
      removeCurrent       : Boolean,
      discardFromRight    : Boolean
    ): CntSorterCellCtrlResult = {
      val updateCell = (currentLessThanInput != discardFromRight) || removeCurrent
      CntSorterCellCtrlResult(
        discardToLeft     = removeCurrent || discardFromRight,
        updateCell        = updateCell,
        shiftFromRight    = currentLessThanInput && updateCell,
        resetFifoPosition = updateCell && ((leftLessThanInput && !currentLessThanInput) || (!rightLessThanInput && currentLessThanInput))
      )
    }
  }

  final class CntSorterCellModel[T <: Data: Real](
    dataType  : T,
    sorterSize: Int,
    index     : Int
  ) {
    private var savedData = minValue(dataType)
    private var fifoPosition = index

    def step(input: CntSorterCellStepInput): CntSorterCellStepResult = {
      require(input.windowSize > 0 && input.windowSize <= sorterSize, s"windowSize must be in [1, $sorterSize], got ${input.windowSize}")
      require(input.state >= 0 && input.state <= 2, s"CntSorterCell state must fit the idle/process/flush encoding, got ${input.state}")

      val currentLessThanInput     = savedData < input.data
      val leftLessThanInput        = if (index == 0) true else input.leftCell.isLessThanInput
      val rightLessThanInput       = if (input.active && !input.lastCell) input.rightCell.isLessThanInput else false
      val effectiveDiscardFromRight = if (input.active && !input.lastCell) input.discardFromRight else false
      val currentDiscard           = fifoPosition == input.windowSize - 1
      val updateCell               = (currentLessThanInput != effectiveDiscardFromRight) || currentDiscard
      val shiftFromRight           = currentLessThanInput && updateCell
      val discardToLeft            = currentDiscard || effectiveDiscardFromRight
      val resetFifoPosition        = updateCell && ((leftLessThanInput && !currentLessThanInput) || (!rightLessThanInput && currentLessThanInput))
      val resetData                = minValue(dataType)

      val (cellData, selectedFifoPosition) =
        if (index == 0) {
          if (input.lastCell) (input.data, 0) else (input.rightCell.sortedData, input.rightCell.fifoPosition)
        } else if (input.lastCell) {
          (input.leftCell.sortedData, input.leftCell.fifoPosition)
        } else if (input.active) {
          if (shiftFromRight) (input.rightCell.sortedData, input.rightCell.fifoPosition) else (input.leftCell.sortedData, input.leftCell.fifoPosition)
        } else {
          (resetData, index)
        }

      val result = CntSorterCellStepResult(
        cellState   = CntSorterCellValue(savedData, fifoPosition, currentLessThanInput),
        dataToLeft  = if (index == 0) None else Some(if (currentLessThanInput) savedData else input.data),
        dataToRight = if (input.active) {
          if (currentLessThanInput) input.data else savedData
        } else {
          savedData
        },
        removeCurrent     = currentDiscard,
        discardToLeft     = if (index == 0) None else Some(discardToLeft),
        updateCell        = updateCell,
        shiftFromRight    = shiftFromRight,
        resetFifoPosition = resetFifoPosition,
        selectedCellData  = cellData
      )

      val registerLoad = input.enableSort && updateCell
      if (input.state == 0) {
        savedData = resetData
      } else if (registerLoad) {
        savedData = cellData
      }

      if (input.enableSort) {
        fifoPosition =
          if (resetFifoPosition) {
            0
          } else if (updateCell) {
            (selectedFifoPosition + 1) % input.windowSize
          } else if (currentDiscard) {
            0
          } else {
            (fifoPosition + 1) % input.windowSize
          }
      }

      result
    }
  }

  def minValue[T <: Data](dataType: T): Double = dataType match {
    case f: FixedPoint =>
      require(f.binaryPoint.known, "FixedPoint tests require a known binary point")
      -math.pow(2.0, f.getWidth - f.binaryPoint.get.toInt - 1)
    case s: SInt =>
      -math.pow(2.0, s.getWidth - 1)
    case _: UInt =>
      0.0
    case other =>
      throw new IllegalArgumentException(s"Unsupported LIS PE model type: ${other.getClass.getName}")
  }
}
