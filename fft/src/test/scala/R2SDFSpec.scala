package opera.fft

/**
 * Runs the shared SDF stage matrix on the radix-2 SDF stage.
 */
class R2SDFSpec extends SDFStageSpec[R2SDF]("R2SDF", 0x5eed2026L) {
  protected def makeDut(params: RadixParams): R2SDF = new R2SDF(params)
  protected def dutIO(dut: R2SDF): RadixIO = dut.io
}
