package opera.fft

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dspblocks._
import dsptools.numbers.DspComplex
import fixedpoint._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper._
import freechips.rocketchip.resources._
import freechips.rocketchip.tilelink._
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

class FFTAXI4(
  address  : AddressSet,
  params   : FFTParams,
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends FFTBlock[
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle
  ](params, beatBytes) with AXI4DspBlock {

  // Generate mem
  override val mem: Option[AXI4RegisterNode] =
    Some(AXI4RegisterNode(address = address, beatBytes))

  // Override regmap if necessary
  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    if (mem.isDefined) mem.get.regmap(mapping: _*)
    else {}
}

class FFTTL(
  address  : AddressSet,
  params   : FFTParams,
  beatBytes: Int = 4
)(implicit p: Parameters)
  extends FFTBlock[
    TLClientPortParameters,
    TLManagerPortParameters,
    TLEdgeOut,
    TLEdgeIn,
    TLBundle
  ](params, beatBytes) with TLDspBlock {

  val device: SimpleDevice = new SimpleDevice("TLFFT", Seq("opera-platform", "TLFFT")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }

  // Generate mem
  override val mem: Option[TLRegisterNode] =
    Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes))

  // Override regmap if necessary
  override def regmap(mapping: (Int, Seq[RegField])*): Unit =
    if (mem.isDefined) mem.get.regmap(mapping: _*)
    else {}
}

abstract class FFTBlock[D, U, E, O, B <: Data](
  params   : FFTParams,
  beatBytes: Int
) extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  // Get input and output widths
  private val inputWidth     : Int = params.inDataType.getWidth
  private val outputWidth    : Int = params.fftOutputType.getWidth
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
      s"The input data width (${in.bits.data.getWidth}) should be the same as calculated one (${8 * inputBeatBytes})."
    )

    // Block
    val fft = Module(new FFT(params))

    // Control & Status registers
    private val stageCount       = log2Ceil(params.fftSize)
    private val hasRuntimeConfig = params.runTime || params.divBy2Reg || params.directionReg
    if (params.divBy2Reg || params.overflowReg) {
      require(
        stageCount <= beatBytes * 8,
        s"FFT stage register vectors require $stageCount bits, but beatBytes=$beatBytes provides only ${beatBytes * 8} bits"
      )
    }

    val w_load_cfg = WireDefault(false.B)
    val r_size     = if (params.runTime)     Some(RegInit(stageCount.U(stageCount.W))) else None
    val r_divBy2   = if (params.divBy2Reg)   Some(RegInit(VecInit(params.stageDivBy2.map(_.B)))) else None
    val r_direction = if (params.directionReg) Some(RegInit(params.direction.B)) else None
    val r_overflow = if (params.overflowReg) Some(RegInit(0.U(stageCount.W))) else None

    // Connect input and output streams
    fft.io.in.valid := in.valid
    fft.io.in.bits  := in.bits.data(inputWidth - 1, 0).asTypeOf(params.inDataType)
    in.ready        := fft.io.in.ready
    fft.io.i_last   := in.bits.last

    out.valid        := fft.io.out.valid
    fft.io.out.ready := out.ready
    out.bits.data    := fft.io.out.bits.asUInt.pad(8 * outputBeatBytes)
    out.bits.last    := fft.io.o_last

    // Connect control registers with adequate IOs
    fft.io.i_load_cfg.foreach(_ := w_load_cfg)
    if (params.runTime)      fft.io.i_size.get        := r_size.get
    if (params.divBy2Reg)    fft.io.i_divBy2.get      := r_divBy2.get
    if (params.directionReg) fft.io.i_fft_or_ifft.get := r_direction.get

    private def writeVec(reg: Vec[Bool], width: Int): RegWriteFn =
      RegWriteFn((valid, data) => {
        when(valid) {
          for (i <- 0 until width) {
            reg(i) := data(i)
          }
        }
        true.B
      })

    private def divBy2Reset: BigInt =
      params.stageDivBy2.zipWithIndex.map {
        case (enabled, index) => if (enabled) BigInt(1) << index else BigInt(0)
      }.sum

    val regs = Regs(beatBytes)
    val mapping = Seq(
      if (params.runTime) {
        Some(regs.sizeLog2 -> RegFieldGroup("size_log2", Some("FFT runtime size control"),
          Seq(
            RegField(r_size.get.getWidth, r_size.get, RegFieldDesc("size_log2", "Active FFT size as log2(number of samples)", reset = Some(stageCount)))
          )
        ))
      } else None,
      if (params.divBy2Reg) {
        Some(regs.divBy2 -> RegFieldGroup("divby2", Some("FFT divide-by-two stage controls"),
          Seq(
            RegField(stageCount, RegReadFn(r_divBy2.get.asUInt), writeVec(r_divBy2.get, stageCount), RegFieldDesc("divby2", "Per-stage divide-by-two controls", reset = Some(divBy2Reset)))
          )
        ))
      } else None,
      if (params.directionReg) {
        Some(regs.direction -> RegFieldGroup("direction", Some("FFT direction control"),
          Seq(
            RegField(1, r_direction.get, RegFieldDesc("direction", "Transform direction: 1 selects FFT, 0 selects IFFT", reset = Some(if (params.direction) 1 else 0)))
          )
        ))
      } else None,
      if (hasRuntimeConfig) {
        Some(regs.loadCfg -> RegFieldGroup("load_cfg", Some("FFT runtime configuration load"),
          Seq(
            RegField.w(1, RegWriteFn((valid, data) => {
              w_load_cfg := valid && data(0)
              true.B
            }), RegFieldDesc("load_cfg", "Write 1 to pulse FFT runtime configuration load", reset = Some(0)))
          )
        ))
      } else None,
      if (params.overflowReg) {
        Some(regs.overflow -> RegFieldGroup("overflow", Some("FFT sticky overflow status"),
          Seq(
            RegField.w1ToClear(stageCount, r_overflow.get, fft.io.overflow.get.asUInt,
              Some(RegFieldDesc("overflow", "Sticky per-stage overflow status; write 1 to clear each bit", reset = Some(0), volatile = true)))
          )
        ))
      } else None
    ).flatten
    // define abstract register map
    if (mapping.nonEmpty) regmap(mapping: _*)
  }
}

//TODO: Add json config.

// AXI4 FFT block
object FFTAXI4App extends App {
  implicit val p: Parameters = Parameters.empty
  private val beatBytes = 4
  private val params = FFTParams(
    inDataType = DspComplex(FixedPoint(16.W, 14.BP)),
    twiddleType = DspComplex(FixedPoint(16.W, 14.BP)),
    fftSize = 512,
    sdfRadix = Radix22,
    runTime = true,
    divBy2Reg = true,
    directionReg = true,
    overflowReg = true,
    numAddPipes = 1,
    numMulPipes = 1,
    useBitReverse = true,
    minSRAMdepth = 8
  )

  private val FFTModule = LazyModule(
    new FFTAXI4(AddressSet(0x500, 0xFF), params, beatBytes)
      with StandaloneAXI4Block {
      override def standaloneParams: AXI4BundleParameters =
        AXI4BundleParameters(
          addrBits = beatBytes*8,
          dataBits = beatBytes*8,
          idBits = 1
        )
      override def dataBytes: Int = math.ceil(params.inDataType.getWidth.toDouble / 8).toInt
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => FFTModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/FFTAXI4"))
  )
}

// TileLink FFT block
object FFTTLApp extends App {
  implicit val p: Parameters = Parameters.empty
  private val beatBytes = 4
  private val params = FFTParams(
    inDataType = DspComplex(FixedPoint(16.W, 14.BP)),
    twiddleType = DspComplex(FixedPoint(16.W, 14.BP)),
    fftSize = 512,
    sdfRadix = Radix22,
    runTime = true,
    divBy2Reg = true,
    directionReg = true,
    overflowReg = true,
    numAddPipes = 1,
    numMulPipes = 1,
    useBitReverse = true,
    minSRAMdepth = 8
  )

  private val FFTModule = LazyModule(
    new FFTTL(AddressSet(0x500, 0xFF), params, beatBytes)
      with StandaloneTLBlock {
      override def standaloneParams: TLBundleParameters =
        TLBundleParameters(
          addressBits = beatBytes * 8,
          dataBits = beatBytes * 8,
          sourceBits = 4,
          sinkBits = 1,
          sizeBits = 2,
          echoFields = Nil,
          requestFields = Nil,
          responseFields = Nil,
          hasBCE = false
        )
      override def dataBytes: Int = math.ceil(params.inDataType.getWidth.toDouble / 8).toInt
    }
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => FFTModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/FFTTL"))
  )
}
