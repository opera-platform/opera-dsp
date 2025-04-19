package preprocessing

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util.{Cat, log2Ceil}
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule, LazyModuleImp, ModuleValue}
import org.chipsalliance.diplomacy.nodes.NodeHandle
import org.chipsalliance.diplomacy.bundlebridge._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}

class SwapIO extends Bundle {
  val r_format: UInt = Input(UInt(2.W))
  val en_swap: Bool = Input(Bool())
}

class Swap(dataBytes: Int) extends LazyModule()(Parameters.empty) with AXI4StreamBlock {

  // AXI4 stream IN/OUT node
  val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val masterNode = AXI4StreamMasterNode(AXI4StreamMasterParameters(
    name = "streamNode", n = dataBytes
  ))
  val streamNode = NodeHandle(slaveNode, masterNode)

  // IO
  lazy val io = IO(new SwapIO)

  lazy val module = new LazyModuleImp(this) {
    val out = masterNode.out.head._1
    val in  = slaveNode.in.head._1
    assert(out.bits.data.getWidth == 2*in.bits.data.getWidth,
      s"The output data width (${out.bits.data.getWidth}) should have double the width of the input (${in.bits.data.getWidth}).")

    val regs = RegInit(0.U(in.bits.data.getWidth.W))
    val cnt = RegInit(0.U(1.W))
    when(in.valid && out.ready) { cnt := Mux(cnt === 1.U, 0.U, cnt +& 1.U) }

    when(in.valid && out.ready && cnt === 0.U) { regs := in.bits.data }

    // Control for data format
    out <> in
    when(io.r_format === 0.U) {
      // Complex 1x (just real part is sent)
      out.bits.data := Cat(0.U((out.bits.data.getWidth/2).W), in.bits.data) // Just pass the real data, fill the upper part of output with zeroes // Complex 2x (both real and imaginary parts are sent)
    }.elsewhen(io.r_format === 1.U) {
      // Complex 2x (both real and imaginary parts are sent)
      when(io.en_swap === 1.U) {
        // Swap places
        out.bits.data := Cat(regs, in.bits.data)
      }.otherwise {
        // Don't swap
        out.bits.data := Cat(in.bits.data, regs)
      }
      out.valid := RegNext(cnt === 1.U) && in.valid
    }.otherwise {
      // Raw data
      out.bits.data := Cat(in.bits.data, regs)
      out.valid := RegNext(cnt === 1.U) && in.valid
    }
  }
}

trait SwapStandalone extends Swap {
  def dataBytes: Int = 4

  val inNode  = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = dataBytes/2)))
  val outNode = BundleBridgeSink[AXI4StreamBundle]()

  outNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = dataBytes/2)) :=
    inNode

  val in  = InModuleBody { inNode.makeIO() }
  val out = InModuleBody { outNode.makeIO() }
}

object SwapApp extends App {
  implicit val p: Parameters = Parameters.empty

  private val SwapModule = LazyModule(
    new Swap(4)
      with SwapStandalone
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => SwapModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./verilog/Swap"))
  )
}
