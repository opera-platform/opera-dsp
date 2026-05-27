package opera.lis

import org.scalatest.flatspec.AnyFlatSpec

/**
 * Self-test for the streaming model.
 */
class LISStreamingModelSpec extends AnyFlatSpec {
  behavior of "LISStreamingModel"

  private def run(samples: Seq[Double], activeSize: Int, maxWindow: Int = 8): Seq[LISStreamingModel.Beat] = {
    val model = new LISStreamingModel(maxWindow, activeSize)
    samples.map(model.accept)
  }

  it should "produce no eviction and stay not-full until the active window fills" in {
    val beats = run(Seq(3.0, 1.0, 2.0), activeSize = 4)
    assert(beats.forall(_.removedFifo.isEmpty))
    assert(beats.forall(!_.full))
  }

  it should "mark full and keep an ascending window once the size is reached" in {
    val beats = run(Seq(3.0, 1.0, 4.0, 2.0), activeSize = 4)
    assert(beats.last.full)
    assert(beats.last.removedFifo.isEmpty)
    assert(beats.last.sortedAfter == Seq(1.0, 2.0, 3.0, 4.0))
  }

  it should "evict the oldest sample first once full" in {
    val beats = run(Seq(3.0, 1.0, 4.0, 2.0, 9.0), activeSize = 4)
    assert(beats.last.removedFifo.contains(3.0))
    assert(beats.last.sortedAfter == Seq(1.0, 2.0, 4.0, 9.0))
  }

  it should "evict the older of two equal values, keeping one duplicate active" in {
    val beats = run(Seq(5.0, 1.0, 5.0, 2.0, 7.0), activeSize = 4)
    assert(beats.last.removedFifo.contains(5.0))
    assert(beats.last.sortedAfter == Seq(1.0, 2.0, 5.0, 7.0))
  }

  it should "keep only the newest activeSize samples for streams longer than twice the window" in {
    val samples = Seq(12.0, 1.0, 9.0, 3.0, 7.0, 2.0, 11.0, 4.0, 6.0)
    val beats   = run(samples, activeSize = 4)
    assert(beats.last.sortedAfter == samples.takeRight(4).sorted)
    assert(beats.forall(_.sortedAfter.forall(v => v == v))) // no NaN
    assert(beats.forall(b => b.sortedAfter == b.sortedAfter.sorted))
  }

  it should "report flushLength equal to the active window size" in {
    assert(new LISStreamingModel(8, 3).flushLength == 3)
    assert(new LISStreamingModel(8, 8).flushLength == 8)
  }

  it should "support an active window of one" in {
    val beats = run(Seq(4.0, 7.0, 2.0), activeSize = 1)
    assert(beats.map(_.sortedAfter) == Seq(Seq(4.0), Seq(7.0), Seq(2.0)))
    assert(beats.head.removedFifo.isEmpty)
    assert(beats(1).removedFifo.contains(4.0))
    assert(beats(2).removedFifo.contains(7.0))
  }
}
