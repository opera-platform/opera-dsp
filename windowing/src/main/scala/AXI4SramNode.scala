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
      supportsRead = TransferSizes.none, //TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))
{
  require(address.contiguous)

  private val addressMask = Utils.addressMaskBits(address, beatBytes)

  def srammap(sram: SyncReadMem[Vec[UInt]]): Unit = {
    val (in, edgeIn) = this.in.head

    val corrupt = if (edgeIn.bundle.requestFields.contains(AMBACorrupt)) {
      Some(SyncReadMem(1 << addressMask.count(identity), UInt(2.W)))
    } else None

    val writeAddress = Cat(addressMask.zip((in.aw.bits.addr >> log2Ceil(beatBytes)).asBools)
      .filter(_._1).map(_._2).reverse)
    val writeSelected = address.contains(in.aw.bits.addr)

    val writeFull = RegInit(false.B)
    val writeId = Reg(UInt())
    val writeEcho = Reg(BundleMap(in.params.echoFields))
    val selectedResponse = RegNext(writeSelected)

    when(in.b.fire) { writeFull := false.B }
    when(in.aw.fire) {
      writeFull := true.B
      writeId := in.aw.bits.id
      selectedResponse := writeSelected
      writeEcho :<= in.aw.bits.echo
    }

    when(in.aw.fire && writeSelected) {
      sram.write(writeAddress, VecInit(Seq(in.w.bits.data)))
      corrupt.foreach {
        _.write(writeAddress, in.w.bits.user(AMBACorrupt).asUInt)
      }
    }

    in.b.valid := writeFull
    in.aw.ready := in.w.valid && (in.b.ready || !writeFull)
    in.w.ready := in.aw.valid && (in.b.ready || !writeFull)

    in.b.bits.id := writeId
    in.b.bits.resp := Mux(selectedResponse, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.echo :<= writeEcho
  }
}
