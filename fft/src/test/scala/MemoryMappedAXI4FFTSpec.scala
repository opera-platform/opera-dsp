package opera.fft

class MemoryMappedAXI4FFTSpec extends MemoryMappedFFTSpec("MemoryMappedAXI4FFT", "axi4", beatBytes = 8, staticFrameSeed = 0xC0FFEEL) {
  protected def runMemoryMappedCheck(params: FFTParams, mmCheck: MemoryMappedFFTCheck): Unit = {
    val lazyDut = MemoryMappedFFTSpecUtils.axi4Dut(address, params, beatBytes)
    test(lazyDut.module)
      .withAnnotations(annotationsFor(params))
      .runPeekPoke(_ => new MemoryMappedAXI4FFTTester(lazyDut, params, address, beatBytes, mmCheck))
  }
}
