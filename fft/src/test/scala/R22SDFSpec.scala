package opera.fft

/**
 * Runs the shared SDF stage matrix on the radix-2^2 SDF stage.
 */
class R22SDFSpec extends SDFStageSpec[SDFStage]("R22SDF", Radix22, 0xC0FFEEL) {
  protected def makeDut(params: RadixParams): SDFStage = new SDFStage(params)
  protected def dutIO(dut: SDFStage): RadixIO = dut.io
}
