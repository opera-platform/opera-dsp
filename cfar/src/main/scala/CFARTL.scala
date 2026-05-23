package opera.cfar

import chisel3._
import dspblocks._
import dsptools.numbers.{BinaryRepresentation, Real}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

class CFARTL[T <: Data: Real: BinaryRepresentation](
  address  : AddressSet,
  params   : CFARParams[T],
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends CFARDspBlock[
    T,
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLEdgeIn,
    TLBundle
  ](params, beatBytes) with TLDspBlock {

  val device: SimpleDevice = new SimpleDevice("TLCFAR", Seq("opera-platform", "TLCFAR"))

  override val mem: Option[TLRegisterNode] =
    Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes))

  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    mem.foreach(_.regmap(mapping: _*))
}
