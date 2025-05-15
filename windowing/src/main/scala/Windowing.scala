package windowing

//import dspblocks.mems.Queue
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
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.resources.{Device, DeviceRegName, DiplomaticSRAM}


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

  val registerNode = Some(AXI4RegisterNode(address = csrAddress, beatBytes))
  val ramNode =
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


  override def srammap(sram: SyncReadMem[Vec[UInt]]) = {
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

abstract class Windowing[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  ramAddress: AddressSet,
  params: WindowingParams[T],
  beatBytes:  Int,
  devName: Option[String] = None,
  dtsCompat: Option[Seq[String]] = None,
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
      Utils.writeWindowFunction2File(fileName = fileName, dataType = params.coeffType, window = windowSeq)

      // Address
      val r_address = RegInit(0.U(log2Ceil(params.numPoints).W))

      // Wires
      val w_in_complex =
        if (params.constWindow)
          in.bits.data.asTypeOf(params.dataType)
        else
          RegNext(in.bits.data.asTypeOf(params.dataType)) // TODO: Postoji li razlog za delay?
      val w_out_complex = Wire(params.dataType.cloneType)

      // RAM declaration
      val ram: Option[SyncReadMem[Vec[UInt]]] = if (!params.constWindow) {
        Some(makeSinglePortedByteWriteSeqMem(
          size = BigInt(1) << mask.filter(b => b).size,
          lanes = beatBytes,
          bits = 8))
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
      val coefficient = if (params.constWindow) rom.get(r_address) else ram.get(r_address).asTypeOf(params.coeffType)

      // Multiplication with the windowing coefficient
      when(r_en) {
        // Windowing is performed only when r_en is active
        DspContext.alter(
          DspContext.current.copy(
            trimType = params.trimType,
            numMulPipes = params.numMulPipes,
            binaryPointGrowth = 0
          )
        ) {
          w_out_complex.real := w_in_complex.real.context_*(coefficient)
          w_out_complex.imag := w_in_complex.imag.context_*(coefficient)
        }
      }.otherwise {
        // Otherwise just pass the input data to the output
        w_out_complex := ShiftRegister(w_in_complex, params.numMulPipes, true.B)
      }

      // Add delay if needed
      if (params.constWindow && params.numMulPipes == 0) {
        out.valid     := in.valid
        out.bits.data := w_out_complex.asUInt
        out.bits.last := in.bits.last
      } else {
        val queueDelay = if (params.constWindow) params.numMulPipes + 1 else params.numMulPipes + 2
        val inputsDelay = if (params.constWindow) params.numMulPipes else params.numMulPipes + 1
        val queueData = Module(new Queue(params.dataType.cloneType, queueDelay, flow = true)) // + 1 for input delaying
        queueData.io.enq.bits := w_out_complex
        queueData.io.enq.valid := ShiftRegister(in.valid, inputsDelay, true.B)
        queueData.io.deq.ready := out.ready

        val queueLast = Module(new Queue(Bool(), queueDelay, flow = true)) // +1 for input delaying
        queueLast.io.enq.valid := ShiftRegister(in.valid, inputsDelay, true.B) // +1 for input delaying
        queueLast.io.enq.bits := ShiftRegister(in.bits.last, inputsDelay, true.B) // +1 for input delaying
        queueLast.io.deq.ready := out.ready

        // Connect output
        out.valid := queueData.io.deq.valid
        out.bits.data := queueData.io.deq.bits.asUInt
        out.bits.last := queueLast.io.deq.bits
      }
      in.ready := out.ready
    } else {
      out <> in
    }
  }
}

object WindowingApp extends App {
  implicit val p: Parameters = Parameters.empty

  val beatBytes = 4
  val paramsWindowing = WindowingParams.fixed(
    numPoints   = 1024,
    dataWidth   = 16,
    binPoint    = 14,
    numMulPipes = 1,
    constWindow = true,
    trimType    = Convergent,
    memoryFile  = "",
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
