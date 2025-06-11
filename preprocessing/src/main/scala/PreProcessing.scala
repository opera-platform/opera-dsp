package opera.preprocessing

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.log2Ceil
import circt.stage.{ChiselStage, FirtoolOption}
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream.AXI4StreamBuffer
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources._
import freechips.rocketchip.tilelink._
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

class PrePrecessingIO extends Bundle {
  val i_crc_data: Bool = Input(Bool())
}

class PreProcessingAXI4(
  address:    AddressSet,
  params:     PreProcessingParameters,
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends PreProcessing[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params)
    with AXI4DspBlock
    with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes))
}

class PreProcessingTL(
  address: AddressSet,
  params:  PreProcessingParameters,
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends PreProcessing[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](params) with TLDspBlock with TLHasCSR {

  val device: SimpleDevice = new SimpleDevice("TLPreProcessing",  Seq("opera-platform", "TLPreProcessing")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem: Some[TLRegisterNode] = Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes))
}


abstract class PreProcessing[D, U, E, O, B <: Data](params: PreProcessingParameters)
  extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  // Block
  val blockCRC     = LazyModule(new CheckerCRC(params.CrcParams, 2*params.MaxChirpSize))
  val blockReverse = LazyModule(new Reverse)
  val blockSwap    = LazyModule(new Swap(outDataWidth = params.CrcParams.dataBytes*2))
  val blockPadder  = LazyModule(new  Padder(MaxChirpSize = params.MaxChirpSize, MaxChirpsPerFrame = params.MaxChirpsPerFrame))

  // Connect in and out streams of the blocks in a chain
  private val blocks = Seq(blockCRC, blockReverse, blockSwap, blockPadder)
  for (i <- 1 until blocks.length) {
    if (params.BufferParams.insertBuffers) {
      blocks(i).streamNode := AXI4StreamBuffer(params.BufferParams.size) := blocks(i - 1).streamNode
    }
    else {
      blocks(i).streamNode := blocks(i - 1).streamNode
    }

  }

  // AXI4Stream node
  val streamNode = NodeHandle(
    blocks.head.streamNode,
    if (params.BufferParams.insertBuffers)
      AXI4StreamBuffer(params.BufferParams.size) := blocks.last.streamNode
    else blocks.last.streamNode
  )
  // IOs
  lazy val io = IO(new PrePrecessingIO)

  lazy val module = new LazyModuleImp(this) {
    // Control & Status registers
    val r_en_crc    = RegInit(false.B)
    val r_en_rev    = RegInit(false.B)
    val r_en_swap   = RegInit(false.B)
    val r_en_zero   = RegInit(false.B)
    val r_crc       = RegInit(0.U(32.W))
    val r_crc_error = RegInit(false.B)
    val r_chirps    = RegInit(1.U(log2Ceil(params.MaxChirpsPerFrame + 1).W))
    val r_format    = RegInit(0.U(2.W))
    val r_samples   = RegInit(params.MaxChirpSize.U(log2Ceil(params.MaxChirpSize + 1).W))
    val r_samples_expected = RegInit(params.MaxChirpSize.U(log2Ceil(params.MaxChirpSize + 1).W))

    // Connect control registers with adequate IOs
    when(blockCRC.io.o_crc_valid) {
      r_crc_error := blockCRC.io.o_error
      r_crc := blockCRC.io.o_crc
    }
    blockCRC.io.i_en                  := r_en_crc
    blockCRC.io.i_crc_data            := io.i_crc_data
    blockCRC.io.i_samples_expected    := Mux(r_format === 0.U, r_samples_expected, r_samples_expected << 1)
    blockReverse.io.i_en              := r_en_rev
    blockSwap.io.i_format             := r_format
    blockSwap.io.i_en                 := r_en_swap
    blockPadder.io.i_samples          := r_samples
    blockPadder.io.i_samples_expected := r_samples_expected
    blockPadder.io.i_chirps           := r_chirps
    blockPadder.io.i_en               := r_en_zero

    val mapping = Seq(
      Regs.chirpsize -> RegFieldGroup("chirpcontrol", Some("Chirp control"),
        Seq(
          RegField(r_samples.getWidth, r_samples, RegFieldDesc("chirpsize", "Number of samples in a chirp", reset = Some(params.MaxChirpSize)))
        )
      ),
      Regs.expectedsize -> RegFieldGroup("chirpexpectedcontrol", Some("Expected Chirp control"),
        Seq(
          RegField(r_samples_expected.getWidth, r_samples_expected, RegFieldDesc("chirpexpected", "Expected number of samples in a chirp", reset = Some(params.MaxChirpSize)))
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
      Regs.crcvalue -> RegFieldGroup("crcvalue", Some("Calculated value from CRC block"),
        Seq(
          RegField.r(r_crc.getWidth, r_crc, RegFieldDesc("crc", "Calculated CRC", reset = Some(0))) // read-only
        )
      ),
      Regs.crcstatus -> RegFieldGroup("crcstatus", Some("CRC status from CRC block"),
        Seq(
          RegField.r(r_crc_error.getWidth, r_crc_error, RegFieldDesc("error", "CRC status", reset = Some(0))) // read-only
        )
      ),
      Regs.ctrl -> RegFieldGroup("blockcontrol", Some("Control of block functionality"),
        Seq(
          RegField(1, r_en_crc,  RegFieldDesc("r_en_crc", "CRC enable", reset = Some(0))),
          RegField(1, r_en_rev,  RegFieldDesc("r_en_rev", "Reverse enable", reset = Some(0))),
          RegField(1, r_en_swap, RegFieldDesc("r_en_swap", "Swap enable", reset = Some(0))),
          RegField(1, r_en_zero, RegFieldDesc("r_en_zero", "Zero padder enable", reset = Some(0))),
        )
      ),
    )
    // define abstract register map
    regmap(mapping: _*)
  }
}

// AXI4 PreProcessing block
object AXI4App extends App {
  implicit val p: Parameters = Parameters.empty
  private val blockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for PreProcessing block.")
      (AddressSet(0x500, 0xFF), PreProcessingParameters())
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ => {
          AppLogger.error("Something went wrong when acquiring DMA Parameters")
          throw new Exception("Invalid configuration")
        }
      }
    }

  private val PreProcessingModule = LazyModule(
    new PreProcessingAXI4(blockParams._1, blockParams._2, beatBytes = 4)
      with StandaloneAXI4Block {
      override def dataBytes: Int = blockParams._2.CrcParams.dataBytes
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => PreProcessingModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/PreProcessingAXI4"))
  )
}

// TileLink PreProcessing block
object TLApp extends App {
  implicit val p: Parameters = Parameters.empty
  private val blockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for PreProcessing block.")
      (AddressSet(0x500, 0xFF), PreProcessingParameters())
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ => {
          AppLogger.error("Something went wrong when acquiring DMA Parameters")
          throw new Exception("Invalid configuration")
        }
      }
    }

  private val PreProcessingModule = LazyModule(
    new PreProcessingTL(blockParams._1, blockParams._2, beatBytes = 4)
      with StandaloneTLBlock {
      override def dataBytes: Int = blockParams._2.CrcParams.dataBytes
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => PreProcessingModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/PreProcessingTL"))
  )
}

