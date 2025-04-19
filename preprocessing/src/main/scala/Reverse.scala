package preprocessing

import chisel3.{Bool, Bundle, IO, Input, when}
import chisel3.util.Reverse
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundle, AXI4StreamIdentityNode, AXI4StreamNodeHandle}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class ReverseIO extends Bundle {
  val en_rev: Bool = Input(Bool())
}

class Reverse extends LazyModule()(Parameters.empty) with AXI4StreamBlock {

  // AXI4 stream IN/OUT node
  val streamNode: AXI4StreamIdentityNode = AXI4StreamIdentityNode()

  // IO
  lazy val io: ReverseIO = IO(new ReverseIO)

  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
    val out: AXI4StreamBundle = streamNode.out.head._1
    val in:  AXI4StreamBundle = streamNode.in.head._1

    out <> in
    when(io.en_rev) {
      out.bits.data := Reverse(in.bits.data)
    }.otherwise {
      out.bits.data := in.bits.data
    }
  }
}
