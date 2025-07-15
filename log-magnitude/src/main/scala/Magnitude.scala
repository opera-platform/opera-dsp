package opera.logmagnitude

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.log2Ceil
import circt.stage.{ChiselStage, FirtoolOption}
import dspblocks._
import dsptools.numbers.{BinaryRepresentation, Real}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBuffer, AXI4StreamBundle, AXI4StreamMasterNode, AXI4StreamMasterParameters, AXI4StreamSlaveNode, AXI4StreamSlaveParameters}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources._
import freechips.rocketchip.tilelink._
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

class MagnitudeAXI4[T <: Data: Real: BinaryRepresentation](
  address  : AddressSet,
  params   : LogMagnitudeParams[T],
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends Magnitude[
    T,
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle
  ](params) with AXI4DspBlock {

  // Generate mem if necessary
  override val mem: Option[AXI4RegisterNode] =
    if (params.magType == LogJPLSquared || params.magType == LogSquaredJPL) {
      Some(AXI4RegisterNode(address = address, beatBytes))
    } else {
      None
    }
  // Override regmap if necessary
  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    if (mem.isDefined) mem.get.regmap(mapping: _*)
    else {}

}

class MagnitudeTL[T <: Data: Real: BinaryRepresentation](
  address  : AddressSet,
  params   : LogMagnitudeParams[T],
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends Magnitude[
    T,
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLEdgeIn,
    TLBundle
  ](params) with TLDspBlock {
  private val device: Option[SimpleDevice] = if (params.magType == LogJPLSquared || params.magType == LogSquaredJPL)
    Some(new SimpleDevice("TLMagnitude",  Seq("opera-platform", "TLMagnitude")) {
      override def describe(resources: ResourceBindings): Description = {
        val Description(name, mapping) = super.describe(resources)
        Description(name, mapping)
      }
    }) else None
  // Generate mem if necessary
  override val mem: Option[TLRegisterNode] =
    if (device.isDefined)
      Some(TLRegisterNode(address = Seq(address), device = device.get, beatBytes = beatBytes))
    else None
  // Override regmap if necessary
  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    if (mem.isDefined) mem.get.regmap(mapping: _*)
    else {}
}


abstract class Magnitude[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](params: LogMagnitudeParams[T])
  extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  // Get input and output widths
  private val inputWidth     : Int = if (params.magType == Log) params.realType.get.getWidth else params.inputType.getWidth
  private val outputWidth    : Int = params.outputType.getWidth
  private val inputBeatBytes : Int = math.ceil(inputWidth.toDouble / 8).toInt
  private val outputBeatBytes: Int = math.ceil(outputWidth.toDouble / 8).toInt

  // AXI4 stream IN/OUT node
  private val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  private val masterNode = AXI4StreamMasterNode(AXI4StreamMasterParameters(
    name = "outNode", n = outputBeatBytes
  ))
  val streamNode = NodeHandle(slaveNode, masterNode)

  lazy val module = new LazyModuleImp(this) {

    val out: AXI4StreamBundle = masterNode.out.head._1
    val in : AXI4StreamBundle = slaveNode.in.head._1
    assert(
      in.bits.data.getWidth == 8 * inputBeatBytes,
      s"The input data width (${in.bits.data.getWidth}) should be the same as calculated one: (${8 * inputBeatBytes})."
    )

    // Optional Control register
    val r_sel: Option[Bool] =
      if (params.magType == LogSquaredJPL || params.magType == LogJPLSquared) Some(RegInit(false.B))
      else None

    // Block
    params.magType match {
      case JPL =>
        val block = Module(new MagnitudeJPL(params))
        block.io.i_last       := in.bits.last
        block.io.in.bits.real := in.bits.data(inputBeatBytes*8-1, inputBeatBytes*4).asTypeOf(block.io.in.bits.real)
        block.io.in.bits.imag := in.bits.data(inputBeatBytes*4-1, 0).asTypeOf(block.io.in.bits.imag)
        block.io.in.valid     := in.valid
        in.ready              := block.io.in.ready
        out.bits.last         := block.io.o_last
        out.bits.data         := block.io.out.bits.asTypeOf(out.bits.data)
        out.valid             := block.io.out.valid
        block.io.out.ready    := out.ready
      case Squared =>
        val block = Module(new MagnitudeSquared(params))
        block.io.i_last       := in.bits.last
        block.io.in.bits.real := in.bits.data(inputBeatBytes * 8 - 1, inputBeatBytes * 4).asTypeOf(block.io.in.bits.real)
        block.io.in.bits.imag := in.bits.data(inputBeatBytes * 4 - 1, 0).asTypeOf(block.io.in.bits.imag)
        block.io.in.valid     := in.valid
        in.ready              := block.io.in.ready
        out.bits.last         := block.io.o_last
        out.bits.data         := block.io.out.bits.asTypeOf(out.bits.data)
        out.valid             := block.io.out.valid
        block.io.out.ready    := out.ready
      case Log =>
        val block = Module(new MagnitudeLog(params))
        block.io.i_last       := in.bits.last
        block.io.in.bits      := in.bits.data.asTypeOf(block.io.in.bits)
        block.io.in.valid     := in.valid
        in.ready              := block.io.in.ready
        out.bits.last         := block.io.o_last
        out.bits.data         := block.io.out.bits.asTypeOf(out.bits.data)
        out.valid             := block.io.out.valid
        block.io.out.ready    := out.ready
      case LogJPLSquared =>
        val block = Module(new MagnitudeMuxed(params))
        block.io.i_last       := in.bits.last
        block.io.in.bits.real := in.bits.data(inputBeatBytes * 8 - 1, inputBeatBytes * 4).asTypeOf(block.io.in.bits.real)
        block.io.in.bits.imag := in.bits.data(inputBeatBytes * 4 - 1, 0).asTypeOf(block.io.in.bits.imag)
        block.io.in.valid     := in.valid
        in.ready              := block.io.in.ready
        out.bits.last         := block.io.o_last
        out.bits.data         := block.io.out.bits.asTypeOf(out.bits.data)
        out.valid             := block.io.out.valid
        block.io.out.ready    := out.ready
        block.io.i_sel.get    := r_sel.get
      case LogSquaredJPL =>
        val block = Module(new MagnitudeMuxed(params))
        block.io.i_last       := in.bits.last
        block.io.in.bits.real := in.bits.data(inputBeatBytes * 8 - 1, inputBeatBytes * 4).asTypeOf(block.io.in.bits.real)
        block.io.in.bits.imag := in.bits.data(inputBeatBytes * 4 - 1, 0).asTypeOf(block.io.in.bits.imag)
        block.io.in.valid     := in.valid
        in.ready              := block.io.in.ready
        out.bits.last         := block.io.o_last
        out.bits.data         := block.io.out.bits.asTypeOf(out.bits.data)
        out.valid             := block.io.out.valid
        block.io.out.ready    := out.ready
        block.io.i_sel.get    := r_sel.get
    }

    val mapping: Option[Seq[(Int, Seq[RegField])]] =
      if (r_sel.isDefined) {
        Some(Seq(
          0 ->
            RegFieldGroup(
              "select",
              Some("Select between magnitude chains"),
              Seq(RegField(r_sel.get.getWidth, r_sel.get, RegFieldDesc("select", "Select between magnitude chains", reset = Some(0))))),
        ))
      } else None
    // define abstract register map
    if (r_sel.isDefined) regmap(mapping.get: _*)
  }
}

// AXI4 Magnitude block
object MagnitudeAXI4App extends App {
  implicit val p: Parameters = Parameters.empty
  private val blockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for Magnitude block.")
      (AddressSet(0x500, 0xFF), LogMagnitudeParams.fixed(), 4)
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ => {
          AppLogger.error("Something went wrong when acquiring Magnitude Parameters")
          throw new Exception("Invalid configuration")
        }
      }
    }

  private val MagnitudeModule = LazyModule(
    new MagnitudeAXI4(blockParams._1, blockParams._2, beatBytes = blockParams._3)
      with StandaloneAXI4Block {
      override def dataBytes: Int = blockParams._3
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => MagnitudeModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeAXI4"))
  )
}

// TileLink Magnitude block
object MagnitudeTLApp extends App {
  implicit val p: Parameters = Parameters.empty
  private val blockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for Magnitude block.")
      (AddressSet(0x500, 0xFF), LogMagnitudeParams.fixed(), 4)
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ => {
          AppLogger.error("Something went wrong when acquiring Magnitude Parameters")
          throw new Exception("Invalid configuration")
        }
      }
    }

  private val MagnitudeModule = LazyModule(
    new MagnitudeTL(blockParams._1, blockParams._2, beatBytes = blockParams._3)
      with StandaloneTLBlock {
      override def dataBytes: Int = blockParams._3
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => MagnitudeModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeTL"))
  )
}

