package opera.lis

object RegSorterModels {
  final case class RegSorterCellTrace(
    data                    : Double,
    keptBeforeInsert        : Boolean,
    nextData                : Option[Double],
    previousKeptBeforeInsert: Boolean,
    previousNextData        : Option[Double]
  )

  final case class RegSorterNetworkTrace(
    sortedData    : Seq[Double],
    removeData    : Double,
    insertData    : Double,
    dataAfterRemove: Seq[Double],
    beforeInsert  : Seq[Boolean],
    nextSortedData: Seq[Double]
  ) {
    def cell(index: Int): RegSorterCellTrace = {
      val lastCellIndex = sortedData.length
      require(index >= 1 && index <= lastCellIndex, s"RegSorterCell index must be in [1, $lastCellIndex], got $index")

      val outputIndex = index - 1
      val isFinal = index == lastCellIndex
      RegSorterCellTrace(
        data                     = nextSortedData(outputIndex),
        keptBeforeInsert         = if (isFinal) false else beforeInsert(outputIndex),
        nextData                 = if (isFinal) None else Some(dataAfterRemove(outputIndex)),
        previousKeptBeforeInsert = if (index == 1) true else beforeInsert(index - 2),
        previousNextData         = if (index > 1 && !isFinal) Some(dataAfterRemove(index - 2)) else None
      )
    }
  }

  object RegSorterNetworkModel {
    def apply(
      sortedData: Seq[Double],
      removeData: Double,
      insertData: Double
    ): RegSorterNetworkTrace = {
      require(sortedData.length >= 2, "RegSorterCell network model needs at least one data cell plus sentinel")
      require(
        sortedData.sliding(2).forall {
          case Seq(left, right) => left <= right
          case _                => true
        },
        s"RegSorterCell input must already be ascending sorted: ${sortedData.mkString(", ")}"
      )

      val removeIndex = sortedData.indexWhere(_ == removeData)
      require(removeIndex >= 0, s"RegSorterCell remove value $removeData was not present in sorted data ${sortedData.mkString(", ")}")

      val dataAfterRemove = sortedData.patch(removeIndex, Nil, 1)
      val nextSortedData  = insertSorted(dataAfterRemove, insertData)
      val beforeInsert    = dataAfterRemove.map(_ < insertData)

      RegSorterNetworkTrace(
        sortedData      = sortedData,
        removeData      = removeData,
        insertData      = insertData,
        dataAfterRemove = dataAfterRemove,
        beforeInsert    = beforeInsert,
        nextSortedData  = nextSortedData
      )
    }
  }

  def insertSorted(values: Seq[Double], value: Double): Seq[Double] = {
    val (before, after) = values.span(_ < value)
    before ++ (value +: after)
  }
}
