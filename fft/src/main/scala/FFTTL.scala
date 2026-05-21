package opera.fft

import chisel3._
import dspblocks._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

class FFTTL(
  address  : AddressSet,
  params   : FFTParams,
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends FFTDspBlock[
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLEdgeIn,
    TLBundle
  ](params, beatBytes) with TLDspBlock {

  val device: SimpleDevice = new SimpleDevice("TLFFT", Seq("opera-platform", "TLFFT"))

  override val mem: Option[TLRegisterNode] =
    Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes))

  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    mem.foreach(_.regmap(mapping: _*))
}
