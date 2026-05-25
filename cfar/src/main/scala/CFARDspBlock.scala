package opera.cfar

import chisel3._
import chisel3.util.{circt => _, _}
import dspblocks._
import dsptools.numbers.{BinaryRepresentation, Real}
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

abstract class CFARDspBlock[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  params   : CFARParams[T],
  beatBytes: Int
) extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  private val inputWidth       = params.inputType.getWidth
  private val thresholdWidth   = params.thresholdType.getWidth
  private val cutWidth         = params.inputType.getWidth
  private val fftBinWidth      = log2Ceil(params.maxFftSize)
  private val outputWidth      = thresholdWidth + fftBinWidth + 1 + (if (params.sendCut) cutWidth else 0)
  private val inputBeatBytes   = math.ceil(inputWidth.toDouble / 8).toInt
  private val outputBeatBytes  = math.ceil(outputWidth.toDouble / 8).toInt
  private val csrDataWidth     = beatBytes * 8
  private val isOrderedStatistic = params.cfarType == CFARType.OrderedStatistic

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

    private val fftSizeWidth      = log2Ceil(params.maxFftSize + 1)
    private val scaleWidth        = params.scaleType.getWidth
    private val referenceWidth    = log2Ceil(params.maxReferenceCells + 1)
    private val guardWidth        = log2Ceil(params.maxGuardCells + 1)
    private val noiseShiftWidth   = log2Ceil(log2Ceil(params.maxReferenceCells + 1))
    private val edgePolicyWidth   = 2

    Seq(
      "fft_size"        -> fftSizeWidth,
      "threshold_scale" -> scaleWidth,
      "peak_grouping"   -> 1,
      "cfar_mode"       -> 2,
      "reference_cells" -> referenceWidth,
      "guard_cells"     -> guardWidth,
      "load_cfg"        -> 1
    ).foreach { case (name, width) =>
      require(width <= csrDataWidth, s"CFAR CSR $name requires $width bits, but beatBytes=$beatBytes provides only $csrDataWidth bits")
    }
    if (!isOrderedStatistic) {
      require(
        noiseShiftWidth <= csrDataWidth,
        s"CFAR CSR noise_div_shift requires $noiseShiftWidth bits, but beatBytes=$beatBytes provides only $csrDataWidth bits"
      )
    }
    if (isOrderedStatistic) {
      require(
        referenceWidth <= csrDataWidth,
        s"CFAR CSR order rank fields require $referenceWidth bits, but beatBytes=$beatBytes provides only $csrDataWidth bits"
      )
    }
    if (params.runtimeLogMode) {
      require(csrDataWidth >= 1, s"CFAR CSR log_mode requires 1 bit, but beatBytes=$beatBytes provides only $csrDataWidth bits")
    }
    if (params.runtimeEdgePolicy) {
      require(
        edgePolicyWidth <= csrDataWidth,
        s"CFAR CSR edge_policy requires $edgePolicyWidth bits, but beatBytes=$beatBytes provides only $csrDataWidth bits"
      )
    }

    val cfar = Module(new CFAR(params))

    val w_load_cfg        = WireDefault(false.B)
    val r_fft_size        = RegInit(params.maxFftSize.U(fftSizeWidth.W))
    val r_threshold_scale = RegInit(0.U(scaleWidth.W))
    val r_peak_grouping   = RegInit(false.B)
    val r_cfar_mode       = RegInit(CFARMode.CellAveraging.U(2.W))
    val r_reference_cells = RegInit(params.maxReferenceCells.U(referenceWidth.W))
    val r_guard_cells     = RegInit(params.maxGuardCells.U(guardWidth.W))
    val r_noise_div_shift = if (!isOrderedStatistic) {
      Some(RegInit(log2Ceil(params.maxReferenceCells).U(noiseShiftWidth.W)))
    } else {
      None
    }
    val r_order_rank_left  = if (isOrderedStatistic) Some(RegInit(1.U(referenceWidth.W))) else None
    val r_order_rank_right = if (isOrderedStatistic) Some(RegInit(1.U(referenceWidth.W))) else None
    val r_log_mode         = if (params.runtimeLogMode) Some(RegInit(params.logMode.B)) else None
    val r_edge_policy      = if (params.runtimeEdgePolicy) Some(RegInit(params.edgePolicy.U(edgePolicyWidth.W))) else None

    cfar.io.i_data.valid := in.valid
    cfar.io.i_data.bits  := in.bits.data(inputWidth - 1, 0).asTypeOf(params.inputType)
    in.ready             := cfar.io.i_data.ready
    cfar.io.i_last       := in.bits.last

    cfar.io.i_load_cfg        := w_load_cfg
    cfar.io.i_fft_size        := r_fft_size
    cfar.io.i_threshold_scale := r_threshold_scale.asTypeOf(params.scaleType)
    cfar.io.i_peak_grouping   := r_peak_grouping
    cfar.io.i_cfar_mode       := r_cfar_mode
    cfar.io.i_reference_cells := r_reference_cells
    cfar.io.i_guard_cells     := r_guard_cells
    cfar.io.i_noise_div_shift.foreach(_ := r_noise_div_shift.get)
    cfar.io.i_order_rank_left.foreach(_ := r_order_rank_left.get)
    cfar.io.i_order_rank_right.foreach(_ := r_order_rank_right.get)
    cfar.io.i_log_mode.foreach(_ := r_log_mode.get)
    cfar.io.i_edge_policy.foreach(_ := r_edge_policy.get)

    out.valid            := cfar.io.o_data.valid
    cfar.io.o_data.ready := out.ready
    out.bits.last        := cfar.io.o_last
    val w_output_payload =
      if (params.sendCut) {
        Cat(
          cfar.io.o_data.bits.threshold.asUInt,
          cfar.io.o_data.bits.cut.get.asUInt,
          cfar.io.o_fft_bin,
          cfar.io.o_data.bits.peak
        )
      } else {
        Cat(
          cfar.io.o_data.bits.threshold.asUInt,
          cfar.io.o_fft_bin,
          cfar.io.o_data.bits.peak
        )
      }
    out.bits.data := w_output_payload.pad(8 * outputBeatBytes)

    val regs = CFARRegs(beatBytes)
    val mapping = Seq(
      Some(regs.fftSize -> RegFieldGroup("fft_size", Some("CFAR active FFT frame size"),
        Seq(
          RegField(fftSizeWidth, r_fft_size,
            RegFieldDesc("fft_size", "Active FFT frame size in samples", reset = Some(params.maxFftSize)))
        )
      )),
      Some(regs.thresholdScale -> RegFieldGroup("threshold_scale", Some("CFAR threshold scale"),
        Seq(
          RegField(scaleWidth, r_threshold_scale,
            RegFieldDesc("threshold_scale", "Raw threshold scale value, interpreted as params.scaleType", reset = Some(0)))
        )
      )),
      Some(regs.peakGrouping -> RegFieldGroup("peak_grouping", Some("CFAR peak grouping control"),
        Seq(
          RegField(1, r_peak_grouping,
            RegFieldDesc("peak_grouping", "Require local maximum when reporting peaks", reset = Some(0)))
        )
      )),
      Some(regs.cfarMode -> RegFieldGroup("cfar_mode", Some("CFAR mode control"),
        Seq(
          RegField(2, r_cfar_mode,
            RegFieldDesc("cfar_mode", "CFAR mode: 0=CA/GOS-CA, 1=GOCA/GOS-GO, 2=SOCA/GOS-SO", reset = Some(CFARMode.CellAveraging)))
        )
      )),
      Some(regs.referenceCells -> RegFieldGroup("reference_cells", Some("CFAR reference window control"),
        Seq(
          RegField(referenceWidth, r_reference_cells,
            RegFieldDesc("reference_cells", "Number of reference cells on each side of the CUT", reset = Some(params.maxReferenceCells)))
        )
      )),
      Some(regs.guardCells -> RegFieldGroup("guard_cells", Some("CFAR guard window control"),
        Seq(
          RegField(guardWidth, r_guard_cells,
            RegFieldDesc("guard_cells", "Number of guard cells on each side of the CUT", reset = Some(params.maxGuardCells)))
        )
      )),
      r_noise_div_shift.map { reg =>
        regs.noiseDivShift -> RegFieldGroup("noise_div_shift", Some("CA-CFAR noise average divisor"),
          Seq(
            RegField(noiseShiftWidth, reg,
              RegFieldDesc("noise_div_shift", "Right-shift applied to each CA-family reference sum", reset = Some(log2Ceil(params.maxReferenceCells))))
          )
        )
      },
      r_order_rank_left.map { reg =>
        regs.orderRankLeft -> RegFieldGroup("order_rank_left", Some("GOS-CFAR left rank selector"),
          Seq(
            RegField(referenceWidth, reg,
              RegFieldDesc("order_rank_left", "One-based ascending rank selected from the left reference window", reset = Some(1)))
          )
        )
      },
      r_order_rank_right.map { reg =>
        regs.orderRankRight -> RegFieldGroup("order_rank_right", Some("GOS-CFAR right rank selector"),
          Seq(
            RegField(referenceWidth, reg,
              RegFieldDesc("order_rank_right", "One-based ascending rank selected from the right reference window", reset = Some(1)))
          )
        )
      },
      r_log_mode.map { reg =>
        regs.logMode -> RegFieldGroup("log_mode", Some("CFAR runtime log/linear mode"),
          Seq(
            RegField(1, reg,
              RegFieldDesc("log_mode", "When runtimeLogMode is enabled, 1 selects log-domain thresholding", reset = Some(if (params.logMode) 1 else 0)))
          )
        )
      },
      r_edge_policy.map { reg =>
        regs.edgePolicy -> RegFieldGroup("edge_policy", Some("CFAR runtime edge policy"),
          Seq(
            RegField(edgePolicyWidth, reg,
              RegFieldDesc("edge_policy", "Runtime edge policy: 0=suppress, 1=one-sided, 2=wrap", reset = Some(params.edgePolicy)))
          )
        )
      },
      Some(regs.loadCfg -> RegFieldGroup("load_cfg", Some("CFAR runtime configuration load"),
        Seq(
          RegField.w(1, RegWriteFn((valid, data) => {
            w_load_cfg := valid && data(0)
            true.B
          }), RegFieldDesc("load_cfg", "Write 1 to pulse CFAR runtime configuration load", reset = Some(0)))
        )
      ))
    ).flatten

    regmap(mapping: _*)
  }
}
