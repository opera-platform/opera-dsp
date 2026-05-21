package opera.fft

import chisel3._
import chisel3.util.{circt => _, _}
import dspblocks._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

abstract class FFTDspBlock[D, U, E, O, B <: Data](
  params   : FFTParams,
  beatBytes: Int
) extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  private val inputWidth     : Int = params.inDataType.getWidth
  private val outputWidth    : Int = params.fftOutputType.getWidth
  private val inputBeatBytes : Int = math.ceil(inputWidth.toDouble / 8).toInt
  private val outputBeatBytes: Int = math.ceil(outputWidth.toDouble / 8).toInt

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

    val fft = Module(new FFT(params))

    private val stageCount       = log2Ceil(params.fftSize)
    private val hasRuntimeConfig = params.runTime || params.divBy2Reg || params.directionReg
    if (params.divBy2Reg || params.overflowReg) {
      require(
        stageCount <= beatBytes * 8,
        s"FFT stage register vectors require $stageCount bits, but beatBytes=$beatBytes provides only ${beatBytes * 8} bits"
      )
    }

    val w_load_cfg = WireDefault(false.B)
    val r_size = if (params.runTime) {
      Some(RegInit(stageCount.U(stageCount.W)))
    } else {
      None
    }
    val r_divBy2 = if (params.divBy2Reg) {
      Some(RegInit(VecInit(params.stageDivBy2.map(_.B))))
    } else {
      None
    }
    val r_direction = if (params.directionReg) {
      Some(RegInit(params.direction.B))
    } else {
      None
    }
    val r_overflow = if (params.overflowReg) {
      Some(RegInit(0.U(stageCount.W)))
    } else {
      None
    }

    fft.io.in.valid := in.valid
    fft.io.in.bits  := in.bits.data(inputWidth - 1, 0).asTypeOf(params.inDataType)
    in.ready        := fft.io.in.ready
    fft.io.i_last   := in.bits.last

    out.valid        := fft.io.out.valid
    fft.io.out.ready := out.ready
    out.bits.data    := fft.io.out.bits.asUInt.pad(8 * outputBeatBytes)
    out.bits.last    := fft.io.o_last

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
            RegField.w1ToClear(stageCount, r_overflow.get, fft.io.o_overflow.get.asUInt,
              Some(RegFieldDesc("overflow", "Sticky per-stage overflow status; write 1 to clear each bit", reset = Some(0), volatile = true)))
          )
        ))
      } else None
    ).flatten

    if (mapping.nonEmpty) {
      regmap(mapping: _*)
    }
  }
}
