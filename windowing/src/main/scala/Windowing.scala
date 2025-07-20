package opera.windowing

import breeze.linalg.{max, min}
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dspblocks._
import dsptools._
import dsptools.numbers._
import fixedpoint._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources.{Device, DeviceRegName, DiplomaticSRAM, SimpleDevice}
import freechips.rocketchip.tilelink.{TLBundle, TLBundleParameters, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLIdentityNode, TLManagerPortParameters, TLRegisterNode, TLXbar}
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes.NodeHandle

/**
 * WindowingAXI4 is AXI4 wrapper for Windowing block.
 *
 * @param csrAddress  The address set for control and status register (CSR) accesses.
 * @param ramAddress  The address set for RAM accesses. Only used if RAM is used instead of ROM.
 * @param errAddress  Optional sequence of address sets for error reporting. Defaults to empty.
 * @param params      Windowing parameters.
 * @param beatBytes   The data bus width in bytes.
 * @param wcorrupt    Enables or disables write corrupt behavior for AXI4 (default: true).
 * @tparam T          Data type
 *
 */
class WindowingAXI4[T <: Data: Real: BinaryRepresentation](
  csrAddress: AddressSet,
  ramAddress: AddressSet,
  errAddress: Seq[AddressSet] = Nil,
  params    : WindowingParams[T],
  beatBytes : Int,
  wcorrupt  : Boolean = true
)(implicit p: Parameters)
  extends Windowing[
    T,
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle] (ramAddress, params, beatBytes)
    with AXI4DspBlock {

  private val registerNode = Some(AXI4RegisterNode(address = csrAddress, beatBytes))
  private val ramNode =
    if (params.constWindow) None
    else {
      Some(AXI4SramNode(
        address   = address,
        errors    = errAddress,
        resources = resources,
        beatBytes = beatBytes,
        wcorrupt  = wcorrupt
      ))
    }


  override def srammap(sram: SyncReadMem[Vec[UInt]]): Unit = {
    if (ramNode.isDefined) ramNode.get.srammap(sram) else ()
  }

  override val mem =
    if (params.constWindow)
      registerNode
    else {
      val node = AXI4IdentityNode()
      val topXbar = AXI4Xbar()
      // Connect nodes
      ramNode.get      := topXbar
      registerNode.get := topXbar
      topXbar          := node
      // Return node
      Some(node)
    }

  override def regmap(mapping: (Int, Seq[RegField])*): Unit = registerNode.get.regmap(mapping: _*)
}

/**
 * WindowingTL is TileLink wrapper for Windowing block.
 *
 * @param csrAddress  AddressSet for control and status register (CSR) accesses.
 * @param ramAddress  The address set for RAM accesses. Only used if RAM is used instead of ROM.
 * @param params      Windowing parameters.
 * @param beatBytes   Data bus width in bytes.
 * @param p           Implicit Parameters object for configuration (Chisel context).
 * @tparam T          Data type.
 *
 */
class WindowingTL[T <: Data: Real: BinaryRepresentation](
  csrAddress: AddressSet,
  ramAddress: AddressSet,
  params    : WindowingParams[T],
  beatBytes : Int
)(implicit p: Parameters)
  extends Windowing[
    T,
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLEdgeIn,
    TLBundle] (ramAddress, params, beatBytes)
    with TLDspBlock {

  private val registerNode = Some(TLRegisterNode(
    address = Seq(csrAddress),
    device = new SimpleDevice("windowing-regs", Nil),
    beatBytes = beatBytes))
  private val ramNode =
    if (params.constWindow) None
    else {
      Some(TLSramNode(
        address   = address,
        resources = resources,
        beatBytes = beatBytes,
        devName  = Some("tlram")
      ))
    }

  override def srammap(sram: SyncReadMem[Vec[UInt]]): Unit = {
    if (ramNode.isDefined) ramNode.get.srammap(sram) else ()
  }

  override val mem =
    if (params.constWindow)
      registerNode
    else {
      val node = TLIdentityNode()
      val topXbar = TLXbar()
      // Connect nodes
      ramNode.get      := topXbar
      registerNode.get := topXbar
      topXbar          := node
      // Return node
      Some(node)
    }

  override def regmap(mapping: (Int, Seq[RegField])*): Unit = registerNode.get.regmap(mapping: _*)
}

/**
 * Abstract base class for windowing operations on streaming or memory-mapped data.
 *
 * This class provides the foundation for implementing windowed DSP operations in hardware.
 * It supports parameterizable CSR, RAM address space, windowing parameters and data bus width.
 * This class must be extended by a concrete bus protocol implementation (e.g., AXI4, TileLink).
 *
 * @param ramAddress   AddressSet for on-chip RAM (if enabled).
 * @param params       Windowing parameters.
 * @param beatBytes    Data bus width in bytes.
 * @param devName      Optional device name string for hardware generation and metadata.
 * @param dtsCompat    Optional sequence of device tree compatibility strings.
 * @param devOverride  Optional device object for register naming and device customization.
 * @param p            Implicit Chisel Parameters object for context.
 *
 */
abstract class Windowing[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  ramAddress : AddressSet,
  params     : WindowingParams[T],
  beatBytes  : Int,
  devName    : Option[String] = None,
  dtsCompat  : Option[Seq[String]] = None,
  devOverride: Option[Device with DeviceRegName] = None
)(implicit p: Parameters) extends DiplomaticSRAM(ramAddress, beatBytes, devName, dtsCompat, devOverride)
  with DspBlock[D, U, E, O, B]
  with HasCSR
  with HasSRAM {

  // Get input and output widths
  private val inputWidth     : Int = params.inputType.getWidth / 2
  private val outputWidth    : Int = params.outputType.getWidth / 2
  private val coeffWidth     : Int = params.coeffType.getWidth
  private val inputBeatBytes : Int = math.ceil(inputWidth.toDouble / 4).toInt
  private val outputBeatBytes: Int = math.ceil(outputWidth.toDouble / 4).toInt

  // Data binary points
  val inputBinPoint = params.inputType.real match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }
  val outputBinPoint = params.outputType.real match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }
  val coeffBinPoint = params.coeffType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }

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
      s"The input data width (${in.bits.data.getWidth}) should be the same as calculated one (${8 * inputBeatBytes})."
    )

    // Control registers
    val r_size = RegInit(params.numPoints.U(log2Ceil(params.numPoints + 1).W))
    val r_en   = RegInit(false.B)

    // Window size
    val w_size = if (params.runTime) r_size else params.numPoints.U

    // Map the registers
    val regMap = Regs(beatBytes)
    val mapping = Seq(
      regMap.chirpsize -> RegFieldGroup("chirpcontrol", Some("Chirp control"),
        Seq(
          RegField(r_size.getWidth, r_size, RegFieldDesc("chirpsize", "Number of samples in a chirp", reset = Some(params.numPoints)))
        )
      ),
      regMap.ctrl -> RegFieldGroup("blockcontrol", Some("Control of block functionality"),
        Seq(
          RegField(1, r_en, RegFieldDesc("r_en", "Windowing enable", reset = Some(0))),
        )
      ),
    )
    // define abstract register map
    regmap(mapping: _*)

    // Check if the window function is defined
    if (params.windowFunc.function.isDefined) {
      // Get window coefficients
      val windowSeq = params.windowFunc.function.get

      // Write window function to text file
      val fileName = if (params.memoryFile.isEmpty) s"./test_run_dir/${params.windowFunc}.txt" else params.memoryFile
      Utils.writeWindowFunction2File(
        fileName  = fileName,
        dataType  = params.coeffType,
        window    = windowSeq,
        dataBytes = params.coeffType.getWidth/8
      )

      // Address
      val r_address = RegInit(0.U(log2Ceil(params.numPoints).W))

      // Wires
      val w_in_complex  = Wire(params.inputType.cloneType)
      val w_out_complex = Wire(params.outputType.cloneType)

      // RAM declaration
      val ram: Option[SyncReadMem[Vec[UInt]]] = if (!params.constWindow) {
        Some(makeSinglePortedByteWriteSeqMem(
          size  = BigInt(1) << mask.count(b => b),
          lanes = 1,
          bits  = params.coeffType.getWidth))
      } else None
      if (ram.isDefined) {
        srammap(ram.get)
        // Initialize RAM
        if (params.memoryFile.trim().nonEmpty) {
          loadMemoryFromFileInline(ram.get, fileName)
        }
      }

      // ROM declaration
      val rom: Option[Vec[T]] = if (params.constWindow) {
        Some(VecInit(windowSeq.map(
          m => { DspContext.withTrimType(Convergent) { params.coeffType.fromDoubleWithFixedWidth(m) }
        })))
      } else None

      // Increment window address
      when(in.fire) { r_address := Mux(r_address === (w_size - 1.U), 0.U, r_address + 1.U) }
      // Get window coefficient
      val coefficient: T = Wire(params.coeffType.cloneType)
        // get coefficients from ROM
        if (params.constWindow) {
          coefficient := rom.get(r_address)
        // get coefficient's from RAM
        } else {
          // Read one RAM word from memory
          coefficient := ram.get(r_address).asTypeOf(params.coeffType)
        }

      // Multiplication with the windowing coefficient
      when(r_en) {
        // Windowing is performed only when r_en is active
        val w_real = DspContext.alter(DspContext.current.copy(trimType = params.trimType, binaryPointGrowth = 0)) {
          val w_mul = w_in_complex.real * coefficient
          val mulBinPoint = w_mul.cloneType match {
            case data: FixedPoint => data.binaryPoint.get
            case _ => 0
          }
          (w_in_complex.real * coefficient).div2(if (mulBinPoint > outputBinPoint) {mulBinPoint - outputBinPoint} else 0)
        }
        val w_imag = DspContext.alter(DspContext.current.copy(trimType = params.trimType, binaryPointGrowth = 0)) {
          val w_mul = w_in_complex.imag * coefficient
          val mulBinPoint = w_mul.cloneType match {
            case data: FixedPoint => data.binaryPoint.get
            case _ => 0
          }
          (w_in_complex.imag * coefficient).div2(if (mulBinPoint > outputBinPoint) mulBinPoint - outputBinPoint else 0)
        }
        w_out_complex.real := w_real.asTypeOf(w_out_complex.imag)
        w_out_complex.imag := w_imag.asTypeOf(w_out_complex.imag)
      }.otherwise {
        // Otherwise just pass the input data to the output
        w_out_complex.real := w_in_complex.real.asTypeOf(w_out_complex.real)
        w_out_complex.imag := w_in_complex.imag.asTypeOf(w_out_complex.imag)
      }
      if (params.constWindow) {
        in.ready      := out.ready
        out.valid     := in.valid
        out.bits.last := in.bits.last
        w_in_complex  := in.bits.data.asTypeOf(params.inputType)
      } else {
        val r_data  = Reg(params.inputType.cloneType)
        val r_valid = RegInit(false.B)
        val r_last  = Reg(Bool())
        when(in.ready) {
          r_data  := in.bits.data.asTypeOf(params.inputType)
          r_valid := in.valid
          r_last  := in.bits.last
        }

        w_in_complex  := r_data
        in.ready      := !r_valid || out.ready
        out.valid     := r_valid
        out.bits.last := r_last
      }
      out.bits.data := w_out_complex.asUInt

    } else {
      out <> in
      out.bits.data := Cat(
        in.bits.data(inputWidth * 2 - 1, inputWidth).asTypeOf(SInt(outputWidth.W)),
        in.bits.data(inputWidth - 1, 0).asTypeOf(SInt(outputWidth.W))
      )
    }
  }
}

/**
 * AXI4App app sets up parameters for a windowing DSP block connected via AXI4,
 * instantiates the module, and invokes the Chisel build process to emit SystemVerilog RTL.
 *
 * It accepts an optional command-line argument to specify a custom configuration;
 * otherwise, it uses default parameters and logs this choice.
 *
 * Behavior:
 * - If no command-line arguments are supplied, default AddressSets and parameters are used.
 * - If an argument is given, it attempts to parse a configuration.
 * - Logs parameterization steps and errors with `AppLogger`.
 *
 * Example usage from command line:
 *
 *     $ sbt "project windowing; runMain windowing.AXI4App windowing/src/main/resources/parameters.json"
 *
 * @note Output files will be written to ./rtl/WindowingAXI4.
 */
object AXI4App extends App {
  implicit val p: Parameters = Parameters.empty

  private val beatBytes = 4
  private val blockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for Windowing block.")
      (AddressSet(0x010000, 0xff), AddressSet(0x000000, 0x0fff), WindowingParams.fixed())
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ =>
          AppLogger.error("Something went wrong when acquiring Windowing Parameters")
          throw new Exception("Invalid configuration")
      }
    }

  private val windowingModule = LazyModule(
    new WindowingAXI4[FixedPoint](
      csrAddress = blockParams._1,
      ramAddress = blockParams._2,
      errAddress = Nil,
      params     = blockParams._3,
      beatBytes  = beatBytes
    ) with StandaloneAXI4Block {
      override def standaloneParams: AXI4BundleParameters =
        AXI4BundleParameters(
          addrBits = beatBytes*8,
          dataBits = beatBytes*8,
          idBits   = 1
        )
      override def dataBytes: Int = 4
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(
      ChiselGeneratorAnnotation(() => windowingModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/WindowingAXI4")
    )
  )
}

/**
 * TLApp app sets up parameters for a windowing DSP block connected via TileLink,
 * instantiates the module, and invokes the Chisel build process to emit SystemVerilog RTL.
 *
 * It accepts an optional command-line argument to specify a custom configuration;
 * otherwise, it uses default parameters and logs this choice.
 *
 * Behavior:
 * - If no command-line arguments are supplied, default AddressSets and parameters are used.
 * - If an argument is given, it attempts to parse a configuration.
 * - Logs parameterization steps and errors with `AppLogger`.
 *
 * Example usage from command line:
 *
 *     $ sbt "project windowing; runMain windowing.TLApp windowing/src/main/resources/parameters.json"
 *
 * @note Output files will be written to ./rtl/WindowingTL.
 */
object TLApp extends App {
  implicit val p: Parameters = Parameters.empty

  private val beatBytes = 4
  private val blockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for Windowing block.")
      (AddressSet(0x010000, 0xff), AddressSet(0x000000, 0x0fff), WindowingParams.fixed())
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ =>
          AppLogger.error("Something went wrong when acquiring Windowing Parameters")
          throw new Exception("Invalid configuration")
      }
    }

  private val windowingModule = LazyModule(
    new WindowingTL[FixedPoint](
      csrAddress = blockParams._1,
      ramAddress = blockParams._2,
      params     = blockParams._3,
      beatBytes  = beatBytes
    ) with StandaloneTLBlock {
      override def standaloneParams: TLBundleParameters =
        TLBundleParameters(
          addressBits    = beatBytes * 8,
          dataBits       = beatBytes * 8,
          sourceBits     = 4,
          sinkBits       = 1,
          sizeBits       = 2,
          echoFields     = Nil,
          requestFields  = Nil,
          responseFields = Nil,
          hasBCE         = false
        )
      override def dataBytes: Int = 4
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(
      ChiselGeneratorAnnotation(() => windowingModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/WindowingTL")
    )
  )
}
