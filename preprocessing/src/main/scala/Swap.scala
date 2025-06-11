package opera.preprocessing

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.Cat
import circt.stage.{ChiselStage, FirtoolOption}
import freechips.rocketchip.amba.axi4stream._
import opera.common.{AXI4StreamBlock, StandaloneAXI4StreamBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}
import org.chipsalliance.diplomacy.nodes.NodeHandle

class SwapIO extends Bundle {
  val i_format: UInt = Input(UInt(2.W))
  val i_en: Bool = Input(Bool())
}

class Swap(outDataWidth: Int) extends LazyModule()(Parameters.empty) with AXI4StreamBlock {

  // AXI4 stream IN/OUT node
  val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val masterNode = AXI4StreamMasterNode(AXI4StreamMasterParameters(
    name = "streamNode", n = outDataWidth
  ))
  val streamNode = NodeHandle(slaveNode, masterNode)

  // IO
  lazy val io = IO(new SwapIO)

  lazy val module = new LazyModuleImp(this) {
    val out = masterNode.out.head._1
    val in  = slaveNode.in.head._1
    assert(
      out.bits.data.getWidth == 2*in.bits.data.getWidth,
      s"The output data width (${out.bits.data.getWidth}) should have double the width of the input (${in.bits.data.getWidth})."
    )

    val r_data    = RegInit(0.U(in.bits.data.getWidth.W))
    val r_counter = RegInit(0.U(1.W))

    when(in.fire) {
      r_data    := in.bits.data
      r_counter := r_counter +& 1.U
    }

    // Control for data format
    out <> in
    when(io.i_format === 0.U) {
      // Complex 1x (just real part is sent)
      out.bits.data := Cat(0.U((out.bits.data.getWidth/2).W), in.bits.data) // Just pass the real data, fill the upper part of output with zeroes // Complex 2x (both real and imaginary parts are sent)
    }.elsewhen(io.i_format === 1.U) {
      // Complex 2x (both real and imaginary parts are sent)
      when(io.i_en === 1.U) {
        // Swap places
        out.bits.data := Cat(r_data, in.bits.data)
      }.otherwise {
        // Don't swap
        out.bits.data := Cat(in.bits.data, r_data)
      }
      out.valid := r_counter === 1.U && in.valid
    }.otherwise {
      // Raw data
      out.bits.data := Cat(in.bits.data, r_data)
      out.valid := r_counter === 1.U && in.valid
    }
  }
}

object SwapApp extends App {
  implicit val p: Parameters = Parameters.empty

  val beatBytes = 4

  private val SwapModule = LazyModule(
    new Swap(beatBytes)
      with StandaloneAXI4StreamBlock {
      override def dataBytes: Int = beatBytes/2
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => SwapModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/Swap"))
  )
}
