package preprocessing

import chisel3._
import chisel3.util.log2Ceil
import dspblocks._
import freechips.rocketchip.amba.axi4stream.AXI4StreamBuffer
import freechips.rocketchip.diplomacy.BufferParams
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

abstract class PreProcessing[D, U, E, O, B <: Data](params: BlockParameters, beatBytes: Int)
  extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  // Block for reordering data bits
  val blockReverse = LazyModule(new Reverse)
  val blockSwap    = LazyModule(new Swap(4))
  val blockPadder  = LazyModule(new  Padder(ChirpSize = params.ChirpSize, ChirpsPerFrame = params.MaxChirpsPerFrame))

  // Connect in and out streams of the blocks in a chain
  val blocks = Seq(blockReverse, blockSwap, blockPadder)
  for (i <- 1 until blocks.length) {
    blocks(i).streamNode := AXI4StreamBuffer(1) := blocks(i - 1).streamNode
  }

  val streamNode = NodeHandle(blocks.head.streamNode, blocks.last.streamNode)

  lazy val module = new LazyModuleImp(this) {

    val r_chirps  = RegInit(1.U(log2Ceil(params.MaxChirpsPerFrame + 1).W))
    val r_format  = RegInit(0.U(2.W))
    val r_samples = RegInit(params.ChirpSize.U(log2Ceil(params.ChirpSize + 1).W))
    val r_samples_expected = RegInit(params.ChirpSize.U(log2Ceil(params.ChirpSize + 1).W))

    val en_crc = RegInit(false.B)
    val en_rev = RegInit(false.B)
    val en_swap = RegInit(false.B)
    val en_zero = RegInit(false.B)

    // Connect control registers with adequate IOs
    blockReverse.io.en_rev := en_rev
    blockSwap.io.r_format := r_format
    blockSwap.io.en_swap := en_swap
    blockPadder.io.i_samples := r_samples
    blockPadder.io.i_samples_expected := r_samples_expected
    blockPadder.io.i_chirps := r_chirps
    blockPadder.io.i_en_zero := en_zero

    val mapping = Seq(
      Regs.chirpsize -> RegFieldGroup("chirpcontrol", Some("Chirp control"),
        Seq(
          RegField(r_samples.getWidth, r_samples, RegFieldDesc("chirpsize", "Number of samples in a chirp", reset = Some(params.ChirpSize)))
        )
      ),
      Regs.expectedsize -> RegFieldGroup("chirpexpectedcontrol", Some("Expected Chirp control"),
        Seq(
          RegField(r_samples_expected.getWidth, r_samples_expected, RegFieldDesc("chirpexpected", "Expected number of samples in a chirp", reset = Some(params.ChirpSize)))
        )
      ),
      Regs.chirpperframe -> RegFieldGroup("framecontrol", Some("Frame control"),
        Seq(
          RegField(r_chirps.getWidth, r_chirps, RegFieldDesc("chirpsize", "Number of chirps in a frame", reset = Some(1)))
        )
      ),
      Regs.dataformat -> RegFieldGroup("formatcontrol", Some("Data format control"),
        Seq(
          RegField(r_format.getWidth, r_format, RegFieldDesc("dataformat", "Current data format", reset = Some(1)))
        )
      ),
      Regs.ctrl -> RegFieldGroup("blockcontrol", Some("Control of block functionality"),
        Seq(
          RegField(1, en_crc,  RegFieldDesc("en_crc", "CRC enable", reset = Some(0))),
          RegField(1, en_rev,  RegFieldDesc("en_rev", "Reverse enable", reset = Some(0))),
          RegField(1, en_swap, RegFieldDesc("en_swap", "Swap enable", reset = Some(0))),
          RegField(1, en_zero, RegFieldDesc("en_zero", "Zero padder enable", reset = Some(0))),
        )
      ),
    )
    // define abstract register map
    regmap(mapping: _*)
  }
}
