package opera.fft

object BitReverseUtils {
  def bitReverse[T](samples: Seq[T]): Vector[T] = {
    val width = chisel3.util.log2Up(samples.length)
    samples.indices.map(i => samples(bitReverseIndex(i, width))).toVector
  }

  def bitReverseIndex(index: Int, width: Int): Int = {
    var in = index
    var out = 0
    for (_ <- 0 until width) {
      out = (out << 1) | (in & 1)
      in = in >> 1
    }
    out
  }

  def bitReverseFrameGroups[T](samples: Seq[T], frameSize: Int): Vector[T] =
    samples.grouped(frameSize).flatMap { group =>
      if (group.length == frameSize) bitReverse(group) else group.toVector
    }.toVector
}
