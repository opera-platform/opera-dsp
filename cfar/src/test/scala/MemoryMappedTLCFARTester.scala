package opera.cfar

import chiseltest.iotesters.PeekPokeTester
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.{OptionalTLMasterModel, TLBundle}
import opera.common.StandaloneTLBlock

class MemoryMappedTLCFARTester(
    dut      : CFARTL[FixedPoint] with StandaloneTLBlock,
    params   : CFARParams[FixedPoint],
    address  : AddressSet,
    beatBytes: Int,
    mmCheck  : MemoryMappedCFARCheck,
) extends PeekPokeTester(dut.module) with OptionalTLMasterModel with MemoryMappedCFARTestUtils {

  override def memTL: Option[TLBundle] = if (dut.ioMem.isDefined) Some(dut.ioMem.get) else None
  protected def csrRead(address: BigInt): BigInt = memReadWord(address, beatBytes)
  protected def csrWrite(address: BigInt, data: BigInt): Unit = memWriteWord(address, data, beatBytes)
  protected def csrBaseAddress: BigInt = address.base
  protected def csrBeatBytes: Int = beatBytes
  protected def busName: String = "tl"

  runMemoryMappedCheck(dut.in, dut.out, params, mmCheck)
}
