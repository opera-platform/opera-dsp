package opera.cfar

import chisel3._
import chisel3.util._
import dsptools.numbers._
import fixedpoint._

private[cfar] object GOSCFARRankUtils {
  private def highBoundaryValue[T <: Data](proto: T): Double = proto match {
    case fixed: FixedPoint =>
      val integerBits = fixed.getWidth - fixed.binaryPoint.get - 1
      math.pow(2.0, integerBits.toDouble) - math.pow(2.0, -fixed.binaryPoint.get.toDouble)
    case sint: SInt =>
      math.pow(2.0, sint.getWidth.toDouble - 1.0) - 1.0
    case uint: UInt =>
      math.pow(2.0, uint.getWidth.toDouble) - 1.0
    case other =>
      throw new IllegalArgumentException(s"Unsupported GOS-CFAR numeric type: ${other.getClass.getName}")
  }

  private def highSentinel[T <: Data: Real](proto: T): T = {
    val sentinel = Wire(proto.cloneType)
    sentinel := Real[T].fromDouble(highBoundaryValue(proto))
    sentinel
  }

  def sortedActiveReferences[T <: Data: Real](
    refs       : Vec[T],
    activeCount: UInt,
    proto      : T
  ): Vec[T] = {
    val sentinel = highSentinel(proto)
    var stage = refs.indices.map { index =>
      val lane = Wire(proto.cloneType)
      lane := Mux(index.U < activeCount, refs(index), sentinel).asTypeOf(proto)
      lane
    }

    if (refs.length > 1) {
      for (_ <- 0 until refs.length - 1) {
        for (index <- 0 until refs.length - 1) {
          val left  = stage(index)
          val right = stage(index + 1)
          val lo    = Wire(proto.cloneType)
          val hi    = Wire(proto.cloneType)
          val swap  = CFARUtils.greaterThan(left, right)
          lo := Mux(swap, right, left).asTypeOf(proto)
          hi := Mux(swap, left, right).asTypeOf(proto)
          stage = stage.updated(index, lo).updated(index + 1, hi)
        }
      }
    }

    VecInit(stage)
  }

  def selectRank[T <: Data: Real](
    refs        : Vec[T],
    activeCount : UInt,
    oneBasedRank: UInt,
    proto       : T
  ): T = CFARUtils.selectRuntimeValue(sortedActiveReferences(refs, activeCount, proto), oneBasedRank)
}
