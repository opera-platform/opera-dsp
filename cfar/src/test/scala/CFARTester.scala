package opera.cfar

import chisel3._
import dsptools.numbers._

private[cfar] object CFARTester {
  /** Pokes the frame-level controls used by the public CFAR module. */
  def configure[T <: Data: Real](
    dut           : CFAR[T],
    cfarMode      : Int,
    thresholdScale: Double = 1.0,
    logMode       : Boolean = false,
    referenceCells: Int = 8,
    guardCells    : Int = 2,
    peakGrouping  : Boolean = false,
    edgePolicy    : Int = CFAREdgePolicy.OneSidedAverage,
    fftSize       : Int = 0,
    loadConfig    : Boolean = true
  ): Unit =
    CFARStreamTestUtils.pokeCommonControls(
      dut            = streamDut(dut),
      cfarMode       = cfarMode,
      thresholdScale = thresholdScale,
      logMode        = logMode,
      referenceCells = referenceCells,
      guardCells     = guardCells,
      peakGrouping   = peakGrouping,
      edgePolicy     = edgePolicy,
      fftSize        = fftSize,
      loadConfig     = loadConfig
    )

  /**
   * Drives one FFT frame through CFAR and checks accepted outputs in FFT-bin order.
   *
   * When expected bins are provided, thresholds and peaks are checked against the bit-accurate model. CUT, bin number, and last are always checked.
   */
  def expectFrame[T <: Data: Real: BinaryRepresentation](
    dut                 : CFAR[T],
    data                : Seq[Double],
    expected            : Option[Seq[CFARModel.ExpectedBin]] = None,
    readyPattern        : Seq[Boolean] = Seq(true),
    randomReadyValidSeed: Option[Long] = None,
    onInputAccepted     : (Int, CFAR[T]) => Unit = (_sampleIndex: Int, _dut: CFAR[T]) => ()
  ): Unit =
    CFARStreamTestUtils.expectFrame(
      dut                    = streamDut(dut),
      frame                  = data,
      expectedBins           = expected,
      readyPattern           = readyPattern,
      randomReadyValidSeed   = randomReadyValidSeed,
      deterministicMaxCycles = 8000,
      randomMaxCycles        = 20000,
      onInputAccepted        = sampleIndex => onInputAccepted(sampleIndex, dut)
    )

  /**
   * Checks that Decoupled output payload fields remain stable while `ready` is low.
   *
   * The test drives one frame until CFAR presents a valid output,  then keeps the downstream side stalled for several cycles and compares the visible payload each cycle.
   */
  def expectOutputStableWhileBackpressured[T <: Data: Real: BinaryRepresentation](
    dut         : CFAR[T],
    data        : Seq[Double],
    stableCycles: Int = 3,
    maxCycles   : Int = 200
  ): Unit =
    CFARStreamTestUtils.expectOutputStableWhileBackpressured(
      dut          = streamDut(dut),
      frame        = data,
      stableCycles = stableCycles,
      maxCycles    = maxCycles
    )

  def expectInputBackpressureWhenFrameBuffersFill[T <: Data: Real: BinaryRepresentation](
    dut      : CFAR[T],
    data     : Seq[Double],
    maxCycles: Int = 1000
  ): Unit =
    CFARStreamTestUtils.expectInputBackpressureWhenFrameBuffersFill(
      dut       = streamDut(dut),
      frame     = data,
      maxCycles = maxCycles
    )

  private def streamDut[T <: Data](dut: CFAR[T]): CFARStreamTestUtils.StreamDut[T] =
    CFARStreamTestUtils.StreamDut(dut.clock, dut.io, dut.params, "CFAR")
}
