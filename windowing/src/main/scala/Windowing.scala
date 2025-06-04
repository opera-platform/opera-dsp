package windowing

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
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._

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

  // AXI4 stream IN/OUT node
  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val in  = streamNode.in.head._1
    val out = streamNode.out.head._1

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
      val w_in_complex  = Wire(params.dataType.cloneType)
      val w_out_complex = Wire(params.dataType.cloneType)

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
      val coefficient =
        // get coefficients from ROM
        if (params.constWindow) {
          rom.get(r_address)
        // get coefficient's from RAM
        } else {
          // Read one RAM word from memory
          ram.get(r_address).asTypeOf(params.coeffType)
        }

      // Multiplication with the windowing coefficient
      when(r_en) {
        // Windowing is performed only when r_en is active
        DspContext.alter(
          DspContext.current.copy(
            trimType = params.trimType,
            binaryPointGrowth = 0
          )
        ) {
          w_out_complex.real := w_in_complex.real.context_*(coefficient)
          w_out_complex.imag := w_in_complex.imag.context_*(coefficient)
        }
      }.otherwise {
        // Otherwise just pass the input data to the output
        w_out_complex := w_in_complex
      }
      if (params.constWindow) {
        in.ready      := out.ready
        out.valid     := in.valid
        out.bits.last := in.bits.last
        w_in_complex  := in.bits.data.asTypeOf(params.dataType)
      } else {
        val r_data  = Reg(params.dataType.cloneType)
        val r_valid = RegInit(false.B)
        val r_last  = Reg(Bool())
        when(in.ready) {
          r_data  := in.bits.data.asTypeOf(params.dataType)
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
    }
  }
}

object AXI4App extends App {
  implicit val p: Parameters = Parameters.empty

  private val beatBytes = 4
  private val paramsWindowing = WindowingParams.fixed(
    numPoints   = 1024,
    dataWidth   = 16,
    binPoint    = 14,
    constWindow = false,
    trimType    = Convergent,
    memoryFile  = "./rtl/WindowingAXI4/window.txt",
    windowFunc  = BlackmanWindow(N=1024, periodic = true)
  )

  private val windowingModule = LazyModule(
    new WindowingAXI4[FixedPoint](
      csrAddress = AddressSet(0x010000, 0xff),
      ramAddress = AddressSet(0x000000, 0x0fff),
      errAddress = Nil,
      params     = paramsWindowing,
      beatBytes  = beatBytes
    ) with StandaloneAXI4Block {
      override def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
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

object TLApp extends App {
  implicit val p: Parameters = Parameters.empty

  private val beatBytes = 4
  private val paramsWindowing = WindowingParams.fixed(
    numPoints   = 1024,
    dataWidth   = 16,
    binPoint    = 14,
    constWindow = false,
    trimType    = Convergent,
    memoryFile  = "",
    windowFunc  = BlackmanWindow(N=1024, periodic = true)
  )

  private val windowingModule = LazyModule(
    new WindowingTL[FixedPoint](
      csrAddress = AddressSet(0x010000, 0xff),
      ramAddress = AddressSet(0x000000, 0x0fff),
      params     = paramsWindowing,
      beatBytes  = beatBytes
    ) with StandaloneTLBlock {
      override def standaloneParams =
        TLBundleParameters(
          addressBits    = beatBytes*8,
          dataBits       = beatBytes*8,
          sourceBits     = 1,
          sinkBits       = 1,
          sizeBits       = 1,
          echoFields     = Seq(),
          requestFields  = Seq(),
          responseFields = Seq(),
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
