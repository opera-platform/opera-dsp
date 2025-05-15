package windowing

object Regs {
  def apply(beatBytes: Int): Regs = {
    new Regs(beatBytes)
  }
}

class Regs private (beatBytes: Int) {
  val chirpsize = 0*beatBytes
  val ctrl      = 1*beatBytes
}
