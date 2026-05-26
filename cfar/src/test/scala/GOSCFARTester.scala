package opera.cfar

import chisel3._
import dsptools.numbers._

private[cfar] object GOSCFARTester {
  def configure[T <: Data: Real](
    dut           : GOSCFAR[T],
    cfarMode      : Int,
    thresholdScale: Double = 1.0,
    logMode       : Boolean = false,
    referenceCells: Int = 4,
    guardCells    : Int = 1,
    orderRankLeft : Int = 1,
    orderRankRight: Int = 1,
    peakGrouping  : Boolean = false,
    edgePolicy    : Int = CFAREdgePolicy.SuppressEdges,
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
      loadConfig     = loadConfig,
      orderRankLeft  = Some(orderRankLeft),
      orderRankRight = Some(orderRankRight)
    )

  def expectFrame[T <: Data: Real: BinaryRepresentation](
    dut: GOSCFAR[T],
    data                : Seq[Double],
    expected            : Seq[CFARModel.ExpectedBin],
    readyPattern        : Seq[Boolean] = Seq(true),
    randomReadyValidSeed: Option[Long] = None,
    onInputAccepted     : (Int, GOSCFAR[T]) => Unit = (_sampleIndex: Int, _dut: GOSCFAR[T]) => ()
  ): Unit =
    CFARStreamTestUtils.expectFrame(
      dut                    = streamDut(dut),
      frame                  = data,
      expectedBins           = Some(expected),
      readyPattern           = readyPattern,
      randomReadyValidSeed   = randomReadyValidSeed,
      deterministicMaxCycles = 12000,
      randomMaxCycles        = 30000,
      onInputAccepted        = sampleIndex => onInputAccepted(sampleIndex, dut)
    )

  def expectOutputStableWhileBackpressured[T <: Data: Real: BinaryRepresentation](
    dut         : GOSCFAR[T],
    data        : Seq[Double],
    stableCycles: Int = 3,
    maxCycles   : Int = 300
  ): Unit =
    CFARStreamTestUtils.expectOutputStableWhileBackpressured(
      dut          = streamDut(dut),
      frame        = data,
      stableCycles = stableCycles,
      maxCycles    = maxCycles
    )

  def expectInputBackpressureWhenFrameBuffersFill[T <: Data: Real: BinaryRepresentation](
    dut      : GOSCFAR[T],
    data     : Seq[Double],
    maxCycles: Int = 1000
  ): Unit =
    CFARStreamTestUtils.expectInputBackpressureWhenFrameBuffersFill(
      dut       = streamDut(dut),
      frame     = data,
      maxCycles = maxCycles
    )

  private def streamDut[T <: Data](dut: GOSCFAR[T]): CFARStreamTestUtils.StreamDut[T] = CFARStreamTestUtils.StreamDut(dut.clock, dut.io, dut.params, "GOS-CFAR")
}
