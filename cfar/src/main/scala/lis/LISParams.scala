package opera.lis

import chisel3._
import chisel3.experimental.requireIsChiselType
import dsptools.numbers._

object LISType {
  val CntBased: String = "CntBased"
  val RegBased: String = "RegBased"

  val all: Seq[String] = Seq(CntBased, RegBased)
}

case class LISParams[T <: Data: Real](
  dataType     : T,
  maxWindowSize: Int,
  sorterType   : String = LISType.CntBased,
  runTime      : Boolean = false,
) {

  requireIsChiselType(dataType, s"($dataType) must be chisel type")
  require(maxWindowSize > 1, s"Sorter size must be > 1")

  def checkSorterType(): Unit =
    require(
      LISType.all.contains(sorterType),
      s"""LIS type must be one of: ${LISType.all.mkString(", ")}"""
    )
}
