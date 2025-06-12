package opera.preprocessing

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundle, AXI4StreamIdentityNode}
import opera.common.{AXI4StreamBlock, StandaloneAXI4StreamBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class CheckerCRCIO(samplesBeforeCRC: Int) extends Bundle {
  val i_en: Bool = Input(Bool())
  val i_crc_data: Bool = Input(Bool())
  val i_samples_expected: UInt = Input(UInt(log2Ceil(samplesBeforeCRC + 1).W))
  val o_crc: UInt = Output(UInt(32.W))
  val o_error: Bool = Output(Bool())
  val o_crc_valid: Bool = Output(Bool())
}

class CheckerCRC(val params: CRCParameters, val samplesBeforeCRC: Int) extends LazyModule()(Parameters.empty) with AXI4StreamBlock {
  // Check input data width
  assert(
    params.dataBytes == 1 || params.dataBytes == 2 || params.dataBytes == 4,
    f"params.dataBytes = ${params.dataBytes}, but supported values are 1, 2 or 4 bytes."
  )

  // AXI4 stream IN/OUT node
  val streamNode: AXI4StreamIdentityNode = AXI4StreamIdentityNode()

  // IOs
  lazy val io = IO(new CheckerCRCIO(samplesBeforeCRC))

  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
    val out: AXI4StreamBundle = streamNode.out.head._1
    val in: AXI4StreamBundle = streamNode.in.head._1

    assert(
      in.bits.data.getWidth == 8 || in.bits.data.getWidth == 16 || in.bits.data.getWidth == 32,
      f"Input stream width is: ${in.bits.data.getWidth}, but supported values are 8, 16 or 32."
    )

    // Additional cycles needed to read input stream CRC
    val cyclesForCRC: Int = 32/in.bits.data.getWidth

    // CRC module
    val crc = Module(new CRC(params))
    // Registers
    val r_crc_counter = if (cyclesForCRC > 1) Some(RegInit(0.U(log2Ceil(cyclesForCRC).W))) else None
    val r_counter = RegInit(0.U(log2Ceil(samplesBeforeCRC + cyclesForCRC).W))
    val r_crc_received = if (cyclesForCRC > 1) Some(Reg(Vec(cyclesForCRC - 1, UInt(in.bits.data.getWidth.W)))) else None

    io.o_crc_valid := false.B
    io.o_error := false.B
    when(in.fire && io.i_en) {
      // count the received data
      r_counter := Mux(r_counter < io.i_samples_expected + (cyclesForCRC - 1).U, r_counter + 1.U, 0.U)

      // Check if CRC data was received
      when((r_counter >= io.i_samples_expected && r_counter <= io.i_samples_expected + (cyclesForCRC - 1).U) || io.i_crc_data) {
        // We can directly compare input CRC and calculated one
        if (cyclesForCRC == 1) {
          io.o_error := crc.io.o_crc =/= in.bits.data
          io.o_crc_valid := true.B
        } // Otherwise we need additional cycles
        else {
          r_crc_counter.get := r_crc_counter.get + 1.U
          when(r_crc_counter.get === (cyclesForCRC - 1).U) {
            io.o_error := crc.io.o_crc =/= Cat(in.bits.data, Cat(r_crc_received.get.reverse))
            io.o_crc_valid := true.B
          }.otherwise {
            if (r_crc_received.get.length > 1) r_crc_received.get(r_crc_counter.get) := in.bits.data
            else r_crc_received.get.head := in.bits.data
          }
        }
      }
    }

    // Connect CRC module
    crc.reset := reset.asBool || !io.i_en
    io.o_crc := crc.io.o_crc
    crc.io.i_en := r_counter < io.i_samples_expected && in.fire
    crc.io.i_data := in.bits.data
    crc.io.i_done := io.o_crc_valid

    // Connect input and output
    out.bits := in.bits
    in.ready := out.ready
    when (io.i_en) {
      out.valid := in.valid && r_counter < io.i_samples_expected
    }.otherwise {
      out.valid := in.valid
    }
  }
}

object CheckerCRCApp extends App {
  implicit val p: Parameters = Parameters.empty

  val beatBytes = 2
  val samplesBeforeCRC = 128

  val params = CRCParameters(
    dataBytes = beatBytes,
    polynomial = 0x04C11DB7,
    init = 0xFFFFFFFFL,
    reflectIn = false,
    reflectOut = false,
    xorOut = 0x00000000L
  )

  private val crcModule = LazyModule(
    new CheckerCRC(params, samplesBeforeCRC)
      with StandaloneAXI4StreamBlock {
      override def dataBytes: Int = beatBytes
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => crcModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/CheckerCRC"))
  )
}

