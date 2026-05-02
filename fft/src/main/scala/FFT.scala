package opera.fft

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import dsptools.numbers._
import fixedpoint._

/**
 * Top-level FFT streaming interface.
 *
 * The input and output streams use Decoupled complex samples. Frame boundaries are carried
 * separately through `i_last` and `o_last`. Runtime configuration and overflow status ports
 * are generated only when the matching fields are enabled in [[FFTParams]].
 *
 * @param params FFT configuration used to size the stream, control, and status signals.
 */
class FFTTopIO(params: FFTParams) extends Bundle {
  private val hasRuntimeConfig = params.runTime || params.divBy2Reg || params.directionReg

  val in:  DecoupledIO[DspComplex[FixedPoint]] = Flipped(Decoupled(params.inDataType))
  val out: DecoupledIO[DspComplex[FixedPoint]] = Decoupled(params.fftOutputType)

  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())

  val i_load_cfg:    Option[Bool]      = if (hasRuntimeConfig) Some(Input(Bool())) else None
  val i_size:        Option[UInt]      = if (params.runTime) Some(Input(UInt(log2Up(params.fftSize).W))) else None
  val i_divBy2:      Option[Vec[Bool]] = if (params.divBy2Reg) Some(Input(Vec(log2Up(params.fftSize), Bool()))) else None
  val i_fft_or_ifft: Option[Bool]      = if (params.directionReg) Some(Input(Bool())) else None

  val overflow: Option[Vec[Bool]] = if (params.overflowReg) Some(Output(Vec(log2Up(params.fftSize), Bool()))) else None
}

object FFTTopIO {
  def apply(params: FFTParams): FFTTopIO = new FFTTopIO(params)
}

/**
 * Top-level streaming single-path delay-feedback FFT.
 *
 * FFT is the wrapper around the radix SDF implementations. The selected
 * radix is controlled by `params.sdfRadix`: [[Radix2]] instantiates [[R2FFT]] and
 * [[Radix22]] instantiates [[R22FFT]]. The radix cores share the Decoupled complex stream interface
 * defined by [[FFTTopIO]], with `i_last` and `o_last` carrying frame boundaries.
 *
 * Runtime controls are present only when enabled in [[FFTParams]]. `i_size` selects the
 * active power-of-two FFT size, `i_divBy2` selects per-stage scaling, `i_fft_or_ifft`
 * selects transform direction, and `i_load_cfg` latches those controls while clearing
 * wrapper-local state.
 *
 * If `useBitReverse` is enabled, [[BitReverse]] is inserted after a DIF core or before a
 * DIT core so that the wrapper exposes natural-order streaming data at both ends. Without
 * bit reversal, the selected radix core exposes its native SDF ordering.
 *
 * @param params FFT hardware parameters, including size, radix, decimation, runtime controls,
 *               scaling, pipeline settings, and optional bit reversal.
 */
class FFT(val params: FFTParams) extends Module {
  val io: FFTTopIO = IO(FFTTopIO(params))

  private val bitReverseSuffix = if (params.useBitReverse) 1 else 0
  override def desiredName: String =
    s"FFT_size_${params.fftSize}_width_${params.inDataType.real.getWidth}_radix_${params.sdfRadix.label}_bitreverse_$bitReverseSuffix"

  private val cfgLoad         = io.i_load_cfg.getOrElse(false.B)
  private val cfgReset        = reset.asBool || cfgLoad
  private val stageCount      = log2Ceil(params.fftSize)
  private val stageCountWidth = log2Ceil(params.fftSize)

  private val activeStageCount = if (params.runTime) {
    val count = RegInit(stageCount.U(stageCountWidth.W))
    when(cfgLoad) { count := io.i_size.get }
    count
  } else {
    stageCount.U(stageCountWidth.W)
  }

  private val fft: HasIO = params.sdfRadix match {
    case Radix2  => Module(new R2FFT(params))
    case Radix22 => Module(new R22FFT(params))
  }

  connectRuntimeConfig(fft)
  connectOverflow(fft)

  if (params.useBitReverse) {
    connectWithBitReverse(fft)
  } else {
    connectDirect(fft)
  }

  private def connectRuntimeConfig(fft: HasIO): Unit = {
    fft.io.i_load_cfg.foreach(_ := cfgLoad)
    if (params.runTime)      fft.io.i_size.get        := io.i_size.get
    if (params.divBy2Reg)    fft.io.i_divBy2.get      := io.i_divBy2.get
    if (params.directionReg) fft.io.i_fft_or_ifft.get := io.i_fft_or_ifft.get
  }

  private def connectOverflow(fft: HasIO): Unit = {
    if (params.overflowReg) {
      io.overflow.get := fft.io.o_overflow.get
    }
  }

  private def bitReverseParams: BitReverseParams =
    BitReverseParams(
      dataType      = if (params.decimation == DIF) params.fftOutputType else params.inDataType,
      memDepth      = params.fftSize,
      runTime       = params.runTime,
      singlePortMem = params.singlePortSRAM
    )

  private def connectDirect(fft: HasIO): Unit = {
    fft.io.in <> io.in
    fft.io.i_last := io.i_last

    io.out <> fft.io.out
    io.o_last := fft.io.o_last
  }

  private def connectWithBitReverse(fft: HasIO): Unit = {
    val bitReverse = withReset(cfgReset) { Module(new BitReverse(bitReverseParams)) }
    if (params.runTime) {
      bitReverse.io.i_samples.get := 1.U << activeStageCount
    }

    if (params.decimation == DIF) {
      fft.io.in <> io.in
      fft.io.i_last := io.i_last

      bitReverse.io.in <> fft.io.out
      bitReverse.io.i_last := fft.io.o_last

      io.out <> bitReverse.io.out
      io.o_last := bitReverse.io.o_last
    } else {
      bitReverse.io.in <> io.in
      bitReverse.io.i_last := io.i_last

      fft.io.in <> bitReverse.io.out
      fft.io.i_last := bitReverse.io.o_last

      io.out <> fft.io.out
      io.o_last := fft.io.o_last
    }
  }
}

object FFTSimpleApp extends App {
  val wordSize = 16
  val binaryPoint = wordSize - 2
  val fftSize = 512
  val isBitReverse = true
  val radix = Radix22

  val params = FFTParams(
    inDataType = DspComplex(FixedPoint(wordSize.W, binaryPoint.BP)),
    twiddleType = DspComplex(FixedPoint(16.W, 14.BP)),
    fftSize = fftSize,
    decimation = DIF,
    useBitReverse = isBitReverse,
    numAddPipes = 1,
    numMulPipes = 1,
    sdfRadix = radix,
    runTime = true,
    minSRAMdepth = 8
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(
      ChiselGeneratorAnnotation(() => new FFT(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/FFT")
    )
  )
}
