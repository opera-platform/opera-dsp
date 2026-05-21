package opera.fft

import chisel3._
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters

class FFTAXI4(
  address  : AddressSet,
  params   : FFTParams,
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends FFTDspBlock[
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle
  ](params, beatBytes) with AXI4DspBlock {

  override val mem: Option[AXI4RegisterNode] =
    Some(AXI4RegisterNode(address = address, beatBytes))

  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    mem.foreach(_.regmap(mapping: _*))
}
