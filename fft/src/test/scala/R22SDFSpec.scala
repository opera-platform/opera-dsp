package opera.fft

/**
 * Runs the shared SDF stage matrix on the radix-2^2 SDF stage.
 */
class R22SDFSpec extends SDFStageSpec[R22SDF]("R22SDF", 0xC0FFEEL) {
  protected def makeDut(params: RadixParams): R22SDF = new R22SDF(params)
  protected def dutIO(dut: R22SDF): RadixIO = dut.io

  override protected def makeModel(params: RadixParams): FFTStageModel = {
    val counterInit = if (params.decimation == DIF) 0 else (params.stageSize / 2 + 1) & (params.stageSize - 1)
    new FFTStageModel(params, Some(counterInit))
  }
}
