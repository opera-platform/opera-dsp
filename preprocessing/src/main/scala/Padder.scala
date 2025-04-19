package preprocessing

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

class PadderIO(ChirpSize: Int, ChirpsPerFrame: Int) extends Bundle {
  val i_samples: UInt = Input(UInt(log2Ceil(ChirpSize+1).W))
  val i_samples_expected: UInt = Input(UInt(log2Ceil(ChirpSize+1).W))
  val i_chirps: UInt = Input(UInt(log2Ceil(ChirpsPerFrame+1).W))
  val i_en_zero: Bool = Input(Bool())
}

class Padder(ChirpSize: Int, ChirpsPerFrame: Int) extends LazyModule()(Parameters.empty) with AXI4StreamBlock {

  // AXI4 stream IN/OUT node
  val streamNode = AXI4StreamIdentityNode()

  // IO
  lazy val io: PadderIO = IO(new PadderIO(ChirpSize, ChirpsPerFrame))

  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
    val out = streamNode.out.head._1
    val in  = streamNode.in.head._1

    val r_sent_samples = RegInit(0.U(log2Ceil(ChirpSize).W))
    val r_sent_chirps = RegInit(0.U(log2Ceil(ChirpsPerFrame).W))

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
        out.bits.last := Mux(io.i_en_zero, w_last_chirp, w_input_last)
        when(out.fire && w_input_last && io.i_en_zero && !(r_sent_samples === io.i_samples - 1.U)) {
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




//class Padder(ChirpSize: Int, ChirpsPerFrame: Int) extends LazyModule()(Parameters.empty) with stream {
//
//  // AXI4 stream IN/OUT node
//  val streamNode = AXI4StreamIdentityNode()
//
//  // IO
//  lazy val io: PadderIO = IO(new PadderIO(ChirpSize, ChirpsPerFrame))
//
//  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
//    val out = streamNode.out.head._1
//    val in  = streamNode.in.head._1
//    dontTouch(io.i_samples_expected)
//
//    val r_sent_samples = RegInit(0.U(log2Ceil(ChirpSize).W))
//    val r_sent_chirps = RegInit(0.U(log2Ceil(ChirpsPerFrame).W))
//
//    val sPass :: sPad :: Nil = Enum(2)
//    val state = RegInit(sPass)
//
//    val w_input_last = Mux(
//      io.i_samples_expected === 0.U, // 0 Samples per chirp is not valid configuration
//      in.bits.last,
//      in.bits.last || (r_sent_samples === io.i_samples_expected - 1.U)
//    )
//    val w_output_last_sample = r_sent_samples === io.i_samples - 1.U
//    val w_last_chirp = w_output_last_sample && r_sent_chirps === io.i_chirps - 1.U
//
//    // Keep track of the samples sent
//    when(out.fire) {
//      r_sent_samples := Mux(
//        r_sent_samples === io.i_samples - 1.U || out.bits.last,
//        0.U,
//        r_sent_samples +& 1.U)
//    }
//    // Keep track of the chirps sent
//    when(out.fire && r_sent_samples === io.i_samples - 1.U) {
//      r_sent_chirps := Mux(r_sent_chirps === io.i_chirps - 1.U, 0.U, r_sent_chirps +& 1.U)
//    }
//
//    // Output control
//    //    switch(state) {
//    // sPass: Pass the input data to the output
//    when(state === sPass) {
//      out <> in
//      out.bits.last := Mux(io.i_en_zero, 0.U, w_input_last)
//      when(out.fire && w_input_last && io.i_en_zero) {
//        state := sPad
//      }
//    }.otherwise {
//      out.bits.data := 0.U
//      out.bits.last := w_last_chirp
//      out.valid := 1.U
//      in.ready := 0.U
//      when(out.fire && w_output_last_sample) {
//        state := sPass
//      }
//    }
//    //    }
//
//  }
//}