package opera.cfar

class MemoryMappedTLCFARSpec extends MemoryMappedCFARSpec("MemoryMappedTLCFAR", "tl", beatBytes = 4) {
  protected def runMemoryMappedCheck(params: CFARParams[fixedpoint.FixedPoint], mmCheck: MemoryMappedCFARCheck): Unit = {
    val lazyDut = MemoryMappedCFARDutFactory.tlDut(address, params, beatBytes)
    test(lazyDut.module)
      .withAnnotations(splitOutputAnnotations)
      .runPeekPoke(_ => new MemoryMappedTLCFARTester(lazyDut, params, address, beatBytes, mmCheck))
  }
}
