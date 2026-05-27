package opera.lis

import chisel3._
import fixedpoint._

import scala.util.Random

object LISStreamScenarios {

  /**
   * Seeded random sample stream whose values are exactly representable by `dataType`, so DUT-vs-model comparison stays bit-exact.
   */
  def randomStream(seed: Long, length: Int, dataType: Data): Seq[Double] = {
    val rng = new Random(seed)
    dataType match {
      case u: UInt =>
        val span = 1 << u.getWidth
        Seq.fill(length)(rng.nextInt(span).toDouble)
      case s: SInt =>
        val span = 1 << s.getWidth
        Seq.fill(length)((rng.nextInt(span) - (span / 2)).toDouble)
      case f: FixedPoint =>
        require(f.binaryPoint.known, "FixedPoint random stream requires a known binary point")
        val span  = 1 << f.getWidth
        val scale  = math.pow(2.0, f.binaryPoint.get.toDouble)
        Seq.fill(length)((rng.nextInt(span) - (span / 2)).toDouble / scale)
      case other =>
        throw new IllegalArgumentException(s"Unsupported LIS random stream type: ${other.getClass.getName}")
    }
  }

  // Shared fixtures consumed by the lis_milovanovic comparison spec.
  final case class SorterUpdatePattern(
    label  : String,
    initial: Seq[Double],
    next   : Double
  )

  val sorterUIntUpdate: SorterUpdatePattern =
    SorterUpdatePattern("uint-basic-update", initial = Seq(9.0, 2.0, 7.0, 4.0), next = 5.0)

  val sorterFlushWindow: Seq[Double]            = Seq(4.0, 1.0, 7.0, 2.0)
  val sorterRandomReadyInitial: Seq[Double]     = Seq(9.0, 1.0, 7.0, 3.0)
  val sorterRandomReadyUpdates: Seq[Double]     = Seq(6.0, 2.0, 8.0)
  val sorterRandomFlushWindow: Seq[Double]      = Seq(5.0, 2.0, 9.0, 1.0)
  val sorterRuntimeFullSize8Window: Seq[Double] = Seq(6.0, 14.0, 1.0, 12.0, 4.0, 8.0, 3.0, 10.0)
}
