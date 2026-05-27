package opera.lis

import scala.collection.immutable.Queue

/**
 * Streaming model for the whole LIS sorter (LIS.md section 4).
 *
 * The model is keyed only on the sequence of accepted input samples and the active window size.  
 * It produces the expected sorted active window, eviction, and full status for each beat, but does not model the internal state of the sorter or the
 *
 * Flush behaviour is not modelled for value. 
 * During a frame flush `o_sorted_data` and `o_data.bits` are architecture-dependent and must not be consumed for rank selection, 
 * so the flush driver checks only `o_last` and output-hold stability. 
 * The model exposes `flushLength` so that driver knows how many `o_last` terminated beats to expect.
 */
object LISStreamingModel {

  /** Expectation produced after one accepted input sample. */
  final case class Beat(
    sortedAfter: Seq[Double],    // ascending active window, valid only while `full`
    removedFifo: Option[Double], // oldest sample evicted this beat (process phase only)
    full       : Boolean         // expected `o_sorter_full` after this beat
  )
}

final class LISStreamingModel(maxWindowSize: Int, activeWindowSize: Int) {
  require(
    activeWindowSize >= 1 && activeWindowSize <= maxWindowSize,
    s"active window size must be in [1, $maxWindowSize], got $activeWindowSize"
  )

  private var window: Queue[Double] = Queue.empty

  /** Accept one input sample and return the resulting per-beat expectation. */
  def accept(sample: Double): LISStreamingModel.Beat = {
    window = window.enqueue(sample)
    val removed =
      if (window.length > activeWindowSize) {
        val (oldest, rest) = window.dequeue
        window = rest
        Some(oldest)
      } else {
        None
      }
    LISStreamingModel.Beat(window.toSeq.sorted, removed, full = window.length == activeWindowSize)
  }

  /** Number of `o_last`-terminated output beats produced by a full-window flush. */
  def flushLength: Int = activeWindowSize
}
