package opera.fft

import chiseltest.iotesters.PeekPokeTester
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.{OptionalTLMasterModel, TLBundle}
import opera.common.StandaloneTLBlock

class MemoryMappedTLFFTTester(
    dut      : FFTTL with StandaloneTLBlock,
    params   : FFTParams,
    address  : AddressSet,
    beatBytes: Int,
    mmCheck  : MemoryMappedFFTCheck,
) extends PeekPokeTester(dut.module) with OptionalTLMasterModel with MemoryMappedFFTTestUtils {

  override def memTL: Option[TLBundle] = if (dut.ioMem.isDefined) Some(dut.ioMem.get) else None
  protected def csrRead(address: BigInt) = memReadWord(address, beatBytes)
  protected def csrWrite(address: BigInt, data: BigInt) = memWriteWord(address, data, beatBytes)
  protected def csrBaseAddress = address.base
  protected def csrBeatBytes = beatBytes
  protected def busName = "tl"
  protected def runtimeFrameSeedBase = 0xC0FFEEL

  runMemoryMappedCheck(dut.in, dut.out, params, mmCheck)
}
