package opera.windowing

import chisel3._
import chisel3.experimental.noPrefix
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util.{circt => _, _}
import dspblocks._
import dsptools.numbers._
import fixedpoint._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources.{Device, DeviceRegName, DiplomaticSRAM, SimpleDevice}
import freechips.rocketchip.tilelink.{TLBundle, TLClientPortParameters, TLEdgeIn, TLEdgeOut, TLIdentityNode, TLManagerPortParameters, TLRegisterNode, TLXbar}
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
    params: WindowingParams[T],
    beatBytes: Int,
    wcorrupt: Boolean = true
)(implicit p: Parameters)
  extends Windowing[
    T,
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle
  ](ramAddress, params, beatBytes)
  with AXI4DspBlock {

  private val registerNode = AXI4RegisterNode(address = csrAddress, beatBytes)
  private val ramNode = Option.when(!params.constWindow)(AXI4SramNode(
    address = address,
    errors = errAddress,
    resources = resources,
    beatBytes = beatBytes,
    wcorrupt = wcorrupt
  ))

  override def srammap(sram: SyncReadMem[Vec[UInt]]): Unit = ramNode.foreach(_.srammap(sram))

  override val mem =
    if (params.constWindow)
      Some(registerNode)
    else {
      val node = AXI4IdentityNode()
      val topXbar = AXI4Xbar()
      // Connect nodes
      ramNode.foreach(_ := topXbar)
      registerNode := topXbar
      topXbar := node
      // Return node
      Some(node)
    }

  override def regmap(mapping: (Int, Seq[RegField])*): Unit = registerNode.regmap(mapping: _*)
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
    params: WindowingParams[T],
    beatBytes: Int
)(implicit p: Parameters)
  extends Windowing[
    T,
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLEdgeIn,
    TLBundle
  ](ramAddress, params, beatBytes)
  with TLDspBlock {

  private val registerNode = TLRegisterNode(
    address = Seq(csrAddress),
    device = new SimpleDevice("windowing-regs", Nil),
    beatBytes = beatBytes)
  private val ramNode = Option.when(!params.constWindow)(TLSramNode(
    address = address,
    resources = resources,
    beatBytes = beatBytes,
    devName = Some("tlram")
  ))

  override def srammap(sram: SyncReadMem[Vec[UInt]]): Unit = ramNode.foreach(_.srammap(sram))

  override val mem =
    if (params.constWindow)
      Some(registerNode)
    else {
      val node = TLIdentityNode()
      val topXbar = TLXbar()
      // Connect nodes
      ramNode.foreach(_ := topXbar)
      registerNode := topXbar
      topXbar := node
      // Return node
      Some(node)
    }

  override def regmap(mapping: (Int, Seq[RegField])*): Unit = registerNode.regmap(mapping: _*)
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
    ramAddress: AddressSet,
    params: WindowingParams[T],
    beatBytes: Int,
    devName: Option[String] = None,
    dtsCompat: Option[Seq[String]] = None,
    devOverride: Option[Device with DeviceRegName] = None
)(implicit p: Parameters)
  extends DiplomaticSRAM(ramAddress, beatBytes, devName, dtsCompat, devOverride)
  with DspBlock[D, U, E, O, B]
  with HasCSR {

  def srammap(sram: SyncReadMem[Vec[UInt]]): Unit

  // Get input and output widths
  private val inputWidth: Int = params.inputType.getWidth / 2
  private val outputWidth: Int = params.outputType.getWidth / 2
  private val coeffWidth: Int = params.coeffType.getWidth
  private val inputBeatBytes: Int = (inputWidth + 3) / 4
  private val outputBeatBytes: Int = (outputWidth + 3) / 4

  // Data binary points
  val inputBinPoint = Utils.binaryPointOf(params.inputType.real)
  val outputBinPoint = Utils.binaryPointOf(params.outputType.real)
  val coeffBinPoint = Utils.binaryPointOf(params.coeffType)

  // AXI4 stream IN/OUT node
  private val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  private val masterNode = AXI4StreamMasterNode(AXI4StreamMasterParameters(
    name = "outNode", n = outputBeatBytes
  ))
  val streamNode = NodeHandle(slaveNode, masterNode)

  lazy val module = new LazyModuleImp(this) {
    val out: AXI4StreamBundle = masterNode.out.head._1
    val in: AXI4StreamBundle = slaveNode.in.head._1

    def alignInputToOutput(value: T): T = {
      val alignedBits = Wire(SInt(outputWidth.W))
      alignedBits := value.asUInt.asSInt << (outputBinPoint - inputBinPoint)
      alignedBits.asTypeOf(params.outputType.real)
    }

    def requantizeProduct(magnitude: UInt, negative: Bool): T = {
      val raw = Mux(negative, ~magnitude +% 1.U, magnitude).asSInt
      val shift = inputBinPoint + coeffBinPoint - outputBinPoint
      if (shift == 0) {
        raw.asUInt(outputWidth - 1, 0).asTypeOf(params.outputType.real)
      } else {
        val floor = raw >> shift
        val fraction = raw.asUInt(shift - 1, 0)
        val half = (BigInt(1) << (shift - 1)).U
        val increment = params.trimType match {
          case Floor      => false.B
          case Ceiling    => fraction.orR
          case Convergent => fraction > half || fraction === half && floor(0)
          case Round      => fraction > half || fraction === half && !raw(raw.getWidth - 1)
          case other      => throw new IllegalArgumentException(s"Unsupported trim type: $other")
        }
        val rounded = Wire(SInt(outputWidth.W))
        rounded := Mux(increment, floor + 1.S, floor)
        rounded.asTypeOf(params.outputType.real)
      }
    }

    assert(
      in.bits.data.getWidth == 8 * inputBeatBytes,
      s"The input data width (${in.bits.data.getWidth}) should be the same as calculated " +
        s"one (${8 * inputBeatBytes})."
    )

    // Control registers
    val runtimeSize = RegInit(params.numPoints.U(log2Ceil(params.numPoints + 1).W))
      .suggestName("r_size")
    val enabled = RegInit(false.B).suggestName("r_en")

    // Window size
    val windowSize = if (params.runTime) runtimeSize else params.numPoints.U

    // Map the registers
    val regMap = Regs(beatBytes)
    val mapping = Seq(
      regMap.chirpsize -> RegFieldGroup("chirpcontrol", Some("Chirp control"),
        Seq(
          RegField(runtimeSize.getWidth, runtimeSize,
            RegFieldDesc("chirpsize", "Number of samples in a chirp",
              reset = Some(params.numPoints)))
        )
      ),
      regMap.ctrl -> RegFieldGroup("blockcontrol", Some("Control of block functionality"),
        Seq(
          RegField(1, enabled,
            RegFieldDesc("r_en", "Windowing enable", reset = Some(0))),
        )
      ),
    )
    // define abstract register map
    regmap(mapping: _*)

    class OutputPayload extends Bundle {
      val data = params.outputType.cloneType
      val last = Bool()
    }

    val roundedInput = Wire(Decoupled(new OutputPayload))
    val windowFunction = params.windowFunc.function
    windowFunction match {
      case Some(windowSeq) =>
        val foldedLength = if (params.windowFunc.periodicity.contains(true)) {
          params.numPoints / 2 + 1
        } else {
          (params.numPoints + 1) / 2
        }
        val storedWindow = if (params.foldSymmetric) windowSeq.take(foldedLength) else windowSeq

        val fileName = if (params.memoryFile.isEmpty) {
          s"./test_run_dir/${params.windowFunc}.txt"
        } else {
          params.memoryFile
        }
        Utils.writeWindowFunction2File(
          fileName = fileName,
          dataType = params.coeffType,
          window = storedWindow
        )

        val coefficientAddress = RegInit(0.U(math.max(1, log2Ceil(params.numPoints)).W))
          .suggestName("r_address")

        class LookupPayload extends Bundle {
          val data = params.inputType.cloneType
          val coefficient = params.coeffType.cloneType
          val last = Bool()
          val enabled = Bool()
        }

        val lookup = Wire(Decoupled(new LookupPayload))

        if (params.constWindow && params.romStyle == Distributed) {
          val memory = Mem(storedWindow.length, UInt(coeffWidth.W)).suggestName("distributedRom")
          loadMemoryFromFileInline(memory, fileName)
          lookup.valid := in.valid
          lookup.bits.data := in.bits.data.asTypeOf(params.inputType)
          lookup.bits.coefficient := memory(coefficientAddress).asTypeOf(params.coeffType)
          lookup.bits.last := in.bits.last
          lookup.bits.enabled := enabled
          in.ready := lookup.ready
        } else {
          val responses = Module(
            new Queue(new LookupPayload, entries = 2, pipe = true, flow = true))
          lookup <> responses.io.deq

          val pending = RegNext(in.fire, false.B)
          val pendingData = RegEnable(in.bits.data.asTypeOf(params.inputType), in.fire)
          val pendingLast = RegEnable(in.bits.last, in.fire)
          val pendingEnable = RegEnable(enabled, in.fire)
          in.ready := responses.io.count + pending.asUInt < 2.U || responses.io.deq.fire

          val readAddress = if (params.foldSymmetric) {
            val mirror = if (params.windowFunc.periodicity.contains(true)) {
              params.numPoints.U - coefficientAddress
            } else {
              (params.numPoints - 1).U - coefficientAddress
            }
            Mux(mirror < coefficientAddress, mirror, coefficientAddress)
          } else {
            coefficientAddress
          }
          val readData = noPrefix {
            if (params.constWindow) {
              val memory = SyncReadMem(storedWindow.length, UInt(coeffWidth.W))
                .suggestName("synchronousRom")
              loadMemoryFromFileInline(memory, fileName)
              memory.read(readAddress, in.fire)
            } else {
              val memory = makeSinglePortedByteWriteSeqMem(
                size = BigInt(1) << mask.count(identity),
                lanes = 1,
                bits = coeffWidth
              ).suggestName("mem_0")
              srammap(memory)
              if (params.memoryFile.trim().nonEmpty) {
                loadMemoryFromFileInline(memory, fileName)
              }
              memory.read(readAddress, in.fire)(0)
            }
          }

          responses.io.enq.valid := pending
          responses.io.enq.bits.data := pendingData
          responses.io.enq.bits.coefficient := readData.asTypeOf(params.coeffType)
          responses.io.enq.bits.last := pendingLast
          responses.io.enq.bits.enabled := pendingEnable
          assert(!pending || responses.io.enq.ready, "Windowing coefficient response overflow")
        }

        when(in.fire) {
          coefficientAddress := Mux(
            in.bits.last || coefficientAddress === (windowSize - 1.U),
            0.U,
            coefficientAddress + 1.U)
        }

        def multiplyMagnitude(sample: T): (UInt, Bool) = {
          val sampleBits = sample.asUInt
          val negative = sampleBits(inputWidth - 1)
          val magnitude = Mux(negative, ~sampleBits +% 1.U, sampleBits)
          (magnitude * lookup.bits.coefficient.asUInt, negative)
        }
        val (realMagnitudeProduct, realNegative) = multiplyMagnitude(lookup.bits.data.real)
        val (imagMagnitudeProduct, imagNegative) = multiplyMagnitude(lookup.bits.data.imag)
        class ProductPayload extends Bundle {
          val realMagnitude = realMagnitudeProduct.cloneType
          val realNegative = Bool()
          val imagMagnitude = imagMagnitudeProduct.cloneType
          val imagNegative = Bool()
          val bypass = params.outputType.cloneType
          val last = Bool()
          val enabled = Bool()
        }

        val productInput = Wire(Decoupled(new ProductPayload))
        productInput.valid := lookup.valid
        productInput.bits.realMagnitude := realMagnitudeProduct
        productInput.bits.realNegative := realNegative
        productInput.bits.imagMagnitude := imagMagnitudeProduct
        productInput.bits.imagNegative := imagNegative
        productInput.bits.bypass.real := alignInputToOutput(lookup.bits.data.real)
        productInput.bits.bypass.imag := alignInputToOutput(lookup.bits.data.imag)
        productInput.bits.last := lookup.bits.last
        productInput.bits.enabled := lookup.bits.enabled
        lookup.ready := productInput.ready

        val product = Queue(productInput, entries = params.mulPipeRegs, pipe = true)

        roundedInput.valid := product.valid
        roundedInput.bits.data.real := Mux(
          product.bits.enabled,
          requantizeProduct(product.bits.realMagnitude, product.bits.realNegative),
          product.bits.bypass.real
        )
        roundedInput.bits.data.imag := Mux(
          product.bits.enabled,
          requantizeProduct(product.bits.imagMagnitude, product.bits.imagNegative),
          product.bits.bypass.imag
        )
        roundedInput.bits.last := product.bits.last
        product.ready := roundedInput.ready

      case None =>
        val bypass = Wire(Decoupled(new OutputPayload))
        val input = in.bits.data.asTypeOf(params.inputType)
        bypass.valid := in.valid
        bypass.bits.data.real := alignInputToOutput(input.real)
        bypass.bits.data.imag := alignInputToOutput(input.imag)
        bypass.bits.last := in.bits.last
        in.ready := bypass.ready

        roundedInput <> Queue(bypass, entries = params.mulPipeRegs, pipe = true)
    }

    val result = Queue(roundedInput, entries = params.roundPipeRegs, pipe = true)

    result.ready := out.ready
    out.valid := result.valid
    out.bits.data := result.bits.data.asUInt
    out.bits.last := result.bits.last

    val capacity = (if (windowFunction.nonEmpty &&
      (!params.constWindow || params.romStyle == Synchronous)) 2 else 0) +
      params.mulPipeRegs + params.roundPipeRegs

    /** Native Chisel ready/valid and occupancy checks. */
    val occupancy = RegInit(0.U(math.max(1, log2Ceil(capacity + 1)).W))
    val stalled = RegNext(out.valid && !out.ready, false.B)
    val heldData = RegEnable(out.bits.data, out.valid && !out.ready)
    val heldLast = RegEnable(out.bits.last, out.valid && !out.ready)

    assert(!(out.fire && occupancy === 0.U && !in.fire),
      "Windowing emitted data without an accepted input")
    assert(!(in.fire && !out.fire && occupancy >= capacity.U),
      "Windowing pipeline occupancy exceeded its capacity")
    assert(!(stalled && (!out.valid || out.bits.data =/= heldData || out.bits.last =/= heldLast)),
      "Windowing output changed while stalled")

    when(in.fire && !out.fire) {
      occupancy := occupancy + 1.U
    }.elsewhen(out.fire && !in.fire) {
      occupancy := occupancy - 1.U
    }
  }
}
