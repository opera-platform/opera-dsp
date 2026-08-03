package opera.windowing

import chisel3._
import chisel3.util.Cat
import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.resources._
import freechips.rocketchip.tilelink._
import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.SinkNode

case class TLSramNode(
  address: AddressSet,
  beatBytes: Int = 4,
  resources: Seq[Resource],
  devName: Option[String] = None
)(implicit valName: ValName)
  extends SinkNode(TLImp)(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address            = List(address),
      resources          = resources,
      regionType         = RegionType.IDEMPOTENT,
      supportsGet        = TransferSizes.none,
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      supportsArithmetic = TransferSizes.none,
      supportsLogical    = TransferSizes.none,
      fifoId             = Some(0)
    ).v2copy(name = devName)), // requests are handled in order
    beatBytes = beatBytes,
    minLatency = 0)
  ))
{
  require(address.contiguous)

  private val addressMask = Utils.addressMaskBits(address, beatBytes)

  def srammap(sram: SyncReadMem[Vec[UInt]]): Unit = {
    val (in, edge) = this.in.head

    val addressBits = addressMask.zip(edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2)
    val memAddress = Cat(addressBits.reverse)

    // "Flow control"
    in.a.ready := in.d.ready
    in.d.valid := in.a.valid

    in.d.bits.source := in.a.bits.source
    in.d.bits.size := in.a.bits.size
    in.d.bits.data := 0.U
    in.d.bits.corrupt := 0.U
    in.d.bits.opcode := TLMessages.AccessAck
    when(in.a.fire && edge.hasData(in.a.bits)) {
      sram.write(memAddress, VecInit(Seq(in.a.bits.data)))
    }

    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}
