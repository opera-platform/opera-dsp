package opera.cfar

import chisel3._
import dspblocks._
import dsptools.numbers.{BinaryRepresentation, Real}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters

class CFARAXI4[T <: Data: Real: BinaryRepresentation](
  address  : AddressSet,
  params   : CFARParams[T],
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends CFARDspBlock[
    T,
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
