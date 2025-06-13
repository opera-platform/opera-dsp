package opera.windowing

import chisel3._
import chisel3.util.{Cat, log2Ceil}
import freechips.rocketchip.amba.AMBACorrupt
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.resources._
import freechips.rocketchip.util._
import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.SinkNode

case class AXI4SramNode(
 address: AddressSet,
 errors: Seq[AddressSet] = Nil,
 resources: Seq[Resource],
 beatBytes: Int = 4,
 wcorrupt: Boolean = true
)(implicit valName: ValName)
  extends SinkNode(AXI4Imp)(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = List(address) ++ errors,
      resources = resources,
      regionType = RegionType.IDEMPOTENT,
      executable = false,
      supportsRead = TransferSizes.none,//TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))
{
  require (address.contiguous)

  private def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)

  private def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(beatBytes))

  def srammap(sram: SyncReadMem[Vec[UInt]]) = {
    val (in, edgeIn) = this.in.head

    val corrupt = if (edgeIn.bundle.requestFields.contains(AMBACorrupt)) Some(SyncReadMem(1 << mask.filter(b => b).size, UInt(2.W))) else None

    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(false.B)
    val w_id = Reg(UInt())
    val w_echo = Reg(BundleMap(in.params.echoFields))
    val w_sel1 = RegNext(w_sel0)

    when(in.b.fire) {
      w_full := false.B
    }
    when(in.aw.fire) {
      w_full := true.B
    }

    when(in.aw.fire) {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      w_echo :<= in.aw.bits.echo
    }

    val wdata = VecInit.tabulate(1) { i => in.w.bits.data }
    when(in.aw.fire && w_sel0) {
      sram.write(w_addr, wdata)
      corrupt.foreach {
        _.write(w_addr, in.w.bits.user(AMBACorrupt).asUInt)
      }
    }

    in.b.valid := w_full
    in.aw.ready := in.w.valid && (in.b.ready || !w_full)
    in.w.ready := in.aw.valid && (in.b.ready || !w_full)

    in.b.bits.id := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.echo :<= w_echo
  }
}

trait HasSRAM {
  def srammap(sram: SyncReadMem[Vec[UInt]]): Unit
}