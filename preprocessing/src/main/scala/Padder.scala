package opera.preprocessing

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import freechips.rocketchip.amba.axi4stream._
import opera.common.{AXI4StreamBlock, StandaloneAXI4StreamBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class PadderIO(MaxChirpSize: Int, MaxChirpsPerFrame: Int) extends Bundle {
  val i_samples: UInt = Input(UInt(log2Ceil(MaxChirpSize+1).W))
  val i_samples_expected: UInt = Input(UInt(log2Ceil(MaxChirpSize+1).W))
  val i_chirps: UInt = Input(UInt(log2Ceil(MaxChirpsPerFrame+1).W))
  val i_en: Bool = Input(Bool())
}

class Padder(MaxChirpSize: Int, MaxChirpsPerFrame: Int) extends LazyModule()(Parameters.empty) with AXI4StreamBlock {

  // AXI4 stream IN/OUT node
  val streamNode = AXI4StreamIdentityNode()

  // IO
  lazy val io: PadderIO = IO(new PadderIO(MaxChirpSize, MaxChirpsPerFrame))

  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
    val out = streamNode.out.head._1
    val in  = streamNode.in.head._1

    val r_sent_samples = RegInit(0.U(log2Ceil(MaxChirpSize).W))
    val r_sent_chirps = RegInit(0.U(log2Ceil(MaxChirpsPerFrame).W))

    val sPass :: sPad :: Nil = Enum(2)
    val state = RegInit(sPass)

    val w_input_last = Mux(
      io.i_samples_expected === 0.U, // 0 Samples per chirp is not valid configuration
      in.bits.last,
      in.bits.last || (r_sent_samples === io.i_samples_expected - 1.U)
    )
    val w_output_last_sample = r_sent_samples === io.i_samples - 1.U
    val w_last_chirp = w_output_last_sample && r_sent_chirps === io.i_chirps - 1.U

    // Keep track of the samples sent
    when(out.fire) {
      r_sent_samples := Mux(
        r_sent_samples === io.i_samples - 1.U || out.bits.last,
        0.U,
        r_sent_samples +& 1.U)
    }
    // Keep track of the chirps sent
    when(out.fire && r_sent_samples === io.i_samples - 1.U) {
      r_sent_chirps := Mux(r_sent_chirps === io.i_chirps - 1.U, 0.U, r_sent_chirps +& 1.U)
    }

    // Output control
    switch(state) {
      // sPass: Pass the input data to the output
      is(sPass) {
        out <> in
        out.bits.last := Mux(io.i_en, w_last_chirp, w_input_last)
        when(out.fire && w_input_last && io.i_en && !(r_sent_samples === io.i_samples - 1.U)) {
          state := sPad
        }
      }
      is(sPad) {
        out.bits.data := 0.U
        out.bits.last := w_last_chirp
        out.valid := 1.U
        in.ready := 0.U
        when(out.fire && w_output_last_sample) {
          state := sPass
        }
      }
    }
  }
}

object PadderApp extends App {
  implicit val p: Parameters = Parameters.empty

  val MaxChirpSize = 1024
  val MaxChirpsPerFrame = 256

  private val padderModule = LazyModule(
    new Padder(MaxChirpSize,MaxChirpsPerFrame)
      with StandaloneAXI4StreamBlock {
      override def dataBytes: Int = 4
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => padderModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/Padder"))
  )
}
