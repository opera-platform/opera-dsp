package opera.fft

class MemoryMappedTLFFTSpec extends MemoryMappedFFTSpec("MemoryMappedTLFFT", "tl", beatBytes = 4, staticFrameSeed = 0xC0FFEEL) {
  protected def runMemoryMappedCheck(params: FFTParams, mmCheck: MemoryMappedFFTCheck): Unit = {
    val lazyDut = MemoryMappedFFTSpecUtils.tlDut(address, params, beatBytes)
    test(lazyDut.module)
      .withAnnotations(annotationsFor(params))
      .runPeekPoke(_ => new MemoryMappedTLFFTTester(lazyDut, params, address, beatBytes, mmCheck))
  }
}
