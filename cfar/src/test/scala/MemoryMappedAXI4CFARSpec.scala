package opera.cfar

class MemoryMappedAXI4CFARSpec extends MemoryMappedCFARSpec("MemoryMappedAXI4CFAR", "axi4", beatBytes = 4) {
  protected def runMemoryMappedCheck(params: CFARParams[fixedpoint.FixedPoint], mmCheck: MemoryMappedCFARCheck): Unit = {
    val lazyDut = MemoryMappedCFARDutFactory.axi4Dut(address, params, beatBytes)
    test(lazyDut.module)
      .withAnnotations(annotations)
      .runPeekPoke(_ => new MemoryMappedAXI4CFARTester(lazyDut, params, address, beatBytes, mmCheck))
  }
}
