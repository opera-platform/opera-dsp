package opera.fft

import chiseltest.iotesters.PeekPokeTester
import freechips.rocketchip.amba.axi4.{AXI4Bundle, OptionalAXI4MasterModel}
import freechips.rocketchip.diplomacy.AddressSet
import opera.common.StandaloneAXI4Block

class MemoryMappedAXI4FFTTester(
    dut      : FFTAXI4 with StandaloneAXI4Block,
    params   : FFTParams,
    address  : AddressSet,
    beatBytes: Int,
    mmCheck  : MemoryMappedFFTCheck,
) extends PeekPokeTester(dut.module) with OptionalAXI4MasterModel with MemoryMappedFFTTestUtils {

  override def memAXI: Option[AXI4Bundle] = if (dut.ioMem.isDefined) Some(dut.ioMem.get) else None
  protected def csrRead(address: BigInt) = memReadWord(address)
  protected def csrWrite(address: BigInt, data: BigInt) = memWriteWord(address, data)
  protected def csrBaseAddress = address.base
  protected def csrBeatBytes = beatBytes
  protected def busName = "axi4"
  protected def runtimeFrameSeedBase = 0xC0FFEEL

  runMemoryMappedCheck(dut.in, dut.out, params, mmCheck)
}
