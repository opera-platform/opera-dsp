package opera.cfar

case class CFARRegs(beatBytes: Int) {
  val fftSize        = 0  * beatBytes
  val thresholdScale = 1  * beatBytes
  val peakGrouping   = 2  * beatBytes
  val cfarMode       = 3  * beatBytes
  val referenceCells = 4  * beatBytes
  val guardCells     = 5  * beatBytes
  val noiseDivShift  = 6  * beatBytes
  val orderRankLeft  = 7  * beatBytes
  val orderRankRight = 8  * beatBytes
  val logMode        = 9  * beatBytes
  val edgePolicy     = 10 * beatBytes
  val loadCfg        = 11 * beatBytes
}
