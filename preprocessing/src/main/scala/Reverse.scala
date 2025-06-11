package opera.preprocessing

import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.Reverse
import chisel3.{Bool, Bundle, IO, Input, when}
import circt.stage.{ChiselStage, FirtoolOption}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundle, AXI4StreamIdentityNode}
import opera.common.{AXI4StreamBlock, StandaloneAXI4StreamBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class ReverseIO extends Bundle {
  val i_en: Bool = Input(Bool())
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
    when(io.i_en) {
      out.bits.data := Reverse(in.bits.data)
    }.otherwise {
      out.bits.data := in.bits.data
    }
  }
}

object ReverseApp extends App {
  implicit val p: Parameters = Parameters.empty

  private val reverseModule = LazyModule(
    new Reverse with StandaloneAXI4StreamBlock {
      override def dataBytes: Int = 2
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => reverseModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/Reverse"))
  )
}
