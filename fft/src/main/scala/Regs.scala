package opera.fft

object Regs {
  def apply(beatBytes: Int): Regs = {
    new Regs(beatBytes)
  }
}

class Regs private (beatBytes: Int) {
  val sizeLog2  = 0 * beatBytes
  val divBy2    = 1 * beatBytes
  val direction = 2 * beatBytes
  val loadCfg   = 3 * beatBytes
  val overflow  = 4 * beatBytes
}
