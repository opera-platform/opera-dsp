package windowing

import chisel3._
import chisel3.util.{Cat, RegEnable, log2Ceil}
import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.SinkNode
import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.resources._
import freechips.rocketchip.tilelink._

case class TLSramNode(
  address: AddressSet,
  beatBytes: Int = 4,
  resources: Seq[Resource],
  devName: Option[String] = None
)(implicit valName: ValName)
  extends SinkNode(TLImp)(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address = List(address),
      resources = resources,
      regionType = RegionType.IDEMPOTENT,
      supportsGet = TransferSizes.none,
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull = TransferSizes(1, beatBytes),
      supportsArithmetic = TransferSizes.none,
      supportsLogical = TransferSizes.none,
      fifoId = Some(0)).v2copy(name = devName)), // requests are handled in order
    beatBytes = beatBytes,
    minLatency = 1)))
{
  require (address.contiguous)

  private def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  private def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(beatBytes))

  def srammap(sram: SyncReadMem[Vec[UInt]]) = {
    val (in, edge) = this.in.head

    val w_addr = Cat((mask zip edge.addr_hi(in.a.bits).asBools).filter(_._1).map(_._2).reverse)

    val hasData = edge.hasData(in.a.bits)
    val wdata = VecInit(Seq.tabulate(1) { i => in.a.bits.data })

    val wen =
      in.a.valid &&
      hasData    &&
      (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData)

    // D channel response: always AccessAck for write
    in.d.valid := RegNext(wen)
    in.d.bits  := RegEnable(edge.AccessAck(in.a.bits), wen)

    // Write to memory
    when(wen) {
      sram.write(w_addr, wdata)
    }

    in.a.ready := true.B
    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}