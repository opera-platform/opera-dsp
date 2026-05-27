package opera.cfar

import chiseltest.iotesters.PeekPokeTester
import fixedpoint._
import freechips.rocketchip.amba.axi4.{AXI4Bundle, OptionalAXI4MasterModel}
import freechips.rocketchip.diplomacy.AddressSet
import opera.common.StandaloneAXI4Block

class MemoryMappedAXI4CFARTester(
    dut      : CFARAXI4[FixedPoint] with StandaloneAXI4Block,
    params   : CFARParams[FixedPoint],
    address  : AddressSet,
    beatBytes: Int,
    mmCheck  : MemoryMappedCFARCheck,
) extends PeekPokeTester(dut.module) with OptionalAXI4MasterModel with MemoryMappedCFARTestUtils {

  override def memAXI: Option[AXI4Bundle] = if (dut.ioMem.isDefined) Some(dut.ioMem.get) else None
  protected def csrRead(address: BigInt): BigInt = memReadWord(address)
  protected def csrWrite(address: BigInt, data: BigInt): Unit = memWriteWord(address, data)
  protected def csrBaseAddress: BigInt = address.base
  protected def csrBeatBytes: Int = beatBytes
  protected def busName: String = "axi4"

  runMemoryMappedCheck(dut.in, dut.out, params, mmCheck)
}
