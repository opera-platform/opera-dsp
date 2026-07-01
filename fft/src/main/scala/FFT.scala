package opera.fft

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import dsptools.numbers._
import fixedpoint._

// TODO: config/zero drain should be handled in a more elegant way, e.g. by using a "drain" signal to the core

/**
 * Top-level streaming single-path delay-feedback FFT.
 *
 * FFT is the wrapper around the radix SDF implementations. The selected
 * radix is controlled by `params.sdfRadix`: [[Radix2]] instantiates [[R2FFT]] and
 * [[Radix22]] instantiates [[R22FFT]]. The radix cores share the Decoupled complex stream interface
 * defined by [[FFTIO]], with `i_last` and `o_last` carrying frame boundaries.
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
  val io: FFTIO = IO(new FFTIO(params))

  private val bitReverseSuffix = if (params.useBitReverse) 1 else 0
  override def desiredName: String =
    s"FFT_size_${params.fftSize}_width_${params.inDataType.real.getWidth}_radix_${params.sdfRadix.label}_bitreverse_$bitReverseSuffix"

  private val rawCfgLoad        = io.i_load_cfg.getOrElse(false.B)
  private val rFrameDrainReload = RegInit(false.B)
  private val cfgLoad           = WireDefault(rawCfgLoad || rFrameDrainReload)
  private val cfgReset          = reset.asBool || cfgLoad
  private val stageCount        = log2Ceil(params.fftSize)
  private val stageCountWidth   = log2Ceil(params.fftSize)

  private val cfgSizeValue = if (params.runTime) {
    val value = Wire(UInt(stageCountWidth.W))
    value := io.i_size.get
    Some(value)
  } else {
    None
  }

  private val cfgDivBy2Value = if (params.divBy2Reg) {
    val value = Wire(Vec(log2Up(params.fftSize), Bool()))
    value := io.i_divBy2.get
    Some(value)
  } else {
    None
  }

  private val cfgDirectionValue = if (params.directionReg) {
    val value = Wire(Bool())
    value := io.i_fft_or_ifft.get
    Some(value)
  } else {
    None
  }

  private val wrapperOutputValid = WireDefault(false.B)
  private val wrapperOutputFire  = WireDefault(false.B)
  private val wrapperOutputLast  = WireDefault(false.B)
  private val wrapperDraining    = WireDefault(false.B)
  private val coreInputLastFire  = WireDefault(false.B)
  private val drainOnLast        = io.i_drain_on_last.getOrElse(false.B)
  private val rFrameDrainPending = RegInit(false.B)
  private val rFrameDrainActive  = RegInit(false.B)
  private val rSuppressAfterDrain = RegInit(false.B)
  private val frameDrainBlocking = rFrameDrainPending || cfgLoad
  private val frameDraining      = rFrameDrainActive && !cfgLoad
  private val zeroDraining       = wrapperDraining || frameDraining
  private val suppressOutput     = rSuppressAfterDrain
  private val startFrameDrain    = drainOnLast && io.in.fire && io.i_last
  private val frameDrainDone     = frameDraining && wrapperOutputFire && wrapperOutputLast

  private val activeStageCount = if (params.runTime) {
    val count = RegInit(stageCount.U(stageCountWidth.W))
    when(cfgLoad) { count := cfgSizeValue.get }
    count
  } else {
    stageCount.U(stageCountWidth.W)
  }
  if (params.useBitReverse) {
    val rOutputFrameActive = RegInit(false.B)
    val outputInProgress   = RegNext(wrapperOutputValid, false.B) || rOutputFrameActive
    val r_cfg_drain_pending    = RegInit(false.B)
    val r_apply_pending_cfg    = RegInit(false.B)
    val applyRawCfg            = rawCfgLoad && !r_cfg_drain_pending && !outputInProgress
    val applyPendingCfg        = r_apply_pending_cfg

    val r_pending_num_stages =
      if (params.runTime) Some(RegInit(stageCount.U(stageCountWidth.W))) else None
    val r_pending_divBy2 =
      if (params.divBy2Reg) Some(RegInit(VecInit(params.stageDivBy2.map(_.B)))) else None
    val r_pending_direction =
      if (params.directionReg) Some(RegInit(params.direction.B)) else None

    cfgLoad := applyRawCfg || applyPendingCfg || rFrameDrainReload
    wrapperDraining := r_cfg_drain_pending && !cfgLoad

    when(applyPendingCfg) {
      r_cfg_drain_pending := false.B
      r_apply_pending_cfg := false.B
    }.elsewhen(rawCfgLoad && !applyRawCfg) {
      if (params.runTime)      r_pending_num_stages.get := io.i_size.get
      if (params.divBy2Reg)    r_pending_divBy2.get     := io.i_divBy2.get
      if (params.directionReg) r_pending_direction.get  := io.i_fft_or_ifft.get
      r_cfg_drain_pending := true.B
      r_apply_pending_cfg := false.B
    }

    if (params.runTime) {
      cfgSizeValue.get := Mux(applyPendingCfg, r_pending_num_stages.get, io.i_size.get)
    }
    if (params.divBy2Reg) {
      cfgDivBy2Value.get := Mux(applyPendingCfg, r_pending_divBy2.get, io.i_divBy2.get)
    }
    if (params.directionReg) {
      cfgDirectionValue.get := Mux(applyPendingCfg, r_pending_direction.get, io.i_fft_or_ifft.get)
    }

    when(cfgLoad) {
      rOutputFrameActive := false.B
    }.elsewhen(wrapperOutputFire && wrapperOutputLast) {
      rOutputFrameActive := false.B
    }.elsewhen(wrapperOutputFire) {
      rOutputFrameActive := true.B
    }

    when(r_cfg_drain_pending && wrapperOutputFire && wrapperOutputLast) {
      r_apply_pending_cfg := true.B
    }
  }

  when(rFrameDrainReload) {
    rFrameDrainReload := false.B
  }

  when(cfgLoad) {
    rFrameDrainPending := false.B
    rFrameDrainActive  := false.B
  }.elsewhen(frameDrainDone) {
    rFrameDrainPending := false.B
    rFrameDrainActive  := false.B
    rFrameDrainReload  := true.B
    rSuppressAfterDrain := true.B
  }.otherwise {
    when(startFrameDrain) {
      rFrameDrainPending := true.B
    }
    when(drainOnLast && coreInputLastFire && (startFrameDrain || rFrameDrainPending)) {
      rFrameDrainActive := true.B
    }
  }
  when(rawCfgLoad) {
    rSuppressAfterDrain := false.B
  }.elsewhen(io.in.fire && !frameDrainBlocking && !zeroDraining) {
    rSuppressAfterDrain := false.B
  }

  private val fft: HasIO = params.sdfRadix match {
    case Radix2  => withReset(coreReset) { Module(new R2FFT(params)) }
    case Radix22 => withReset(coreReset) { Module(new R22FFT(params)) }
  }

  connectRuntimeConfig(fft)
  if (params.overflowReg) {
    io.o_overflow.get <> fft.io.o_overflow.get
  }

  if (params.useBitReverse) {
    connectWithBitReverse(fft)
  } else {
    connectDirect(fft)
  }

  private def connectRuntimeConfig(fft: HasIO): Unit = {
    fft.io.i_load_cfg.foreach(_ := cfgLoad)
    if (params.runTime)      fft.io.i_size.get        := cfgSizeValue.get
    if (params.divBy2Reg)    fft.io.i_divBy2.get      := cfgDivBy2Value.get
    if (params.directionReg) fft.io.i_fft_or_ifft.get := cfgDirectionValue.get
    fft.io.i_drain_on_last.foreach(_ := false.B)
  }

  private def coreReset: Bool =
    if (params.runTime) reset.asBool else reset.asBool || rFrameDrainReload

  private def bitReverseParams: BitReverseParams =
    BitReverseParams(
      dataType      = if (params.decimation == DIF) params.fftOutputType else params.inDataType,
      memDepth      = params.fftSize,
      runTime       = params.runTime,
      singlePortMem = params.singlePortSRAM
    )

  private def connectDirect(fft: HasIO): Unit = {
    fft.io.in.valid := zeroDraining || (io.in.valid && !frameDrainBlocking)
    fft.io.in.bits  := Mux(zeroDraining, 0.U.asTypeOf(params.inDataType), io.in.bits)
    fft.io.i_last   := Mux(zeroDraining, false.B, io.i_last)
    io.in.ready     := !zeroDraining && !frameDrainBlocking && fft.io.in.ready

    io.out.valid := !suppressOutput && fft.io.out.valid
    fft.io.out.ready := suppressOutput || io.out.ready
    io.out.bits := fft.io.out.bits
    io.o_last := !suppressOutput && fft.io.o_last
    wrapperOutputValid := io.out.valid
    wrapperOutputFire  := io.out.fire
    wrapperOutputLast  := io.o_last
    coreInputLastFire  := !zeroDraining && fft.io.in.fire && io.i_last
  }

  private def connectWithBitReverse(fft: HasIO): Unit = {
    val bitReverse = withReset(cfgReset) { Module(new BitReverse(bitReverseParams)) }
    if (params.runTime) {
      bitReverse.io.i_samples.get := 1.U << activeStageCount
    }

    if (params.decimation == DIF) {
      fft.io.in.valid := zeroDraining || (io.in.valid && !frameDrainBlocking)
      fft.io.in.bits  := Mux(zeroDraining, 0.U.asTypeOf(params.inDataType), io.in.bits)
      fft.io.i_last   := Mux(zeroDraining, false.B, io.i_last)
      io.in.ready     := !zeroDraining && !frameDrainBlocking && fft.io.in.ready

      bitReverse.io.in <> fft.io.out
      bitReverse.io.i_last := fft.io.o_last

      io.out.valid := !suppressOutput && bitReverse.io.out.valid
      bitReverse.io.out.ready := suppressOutput || io.out.ready
      io.out.bits := bitReverse.io.out.bits
      io.o_last := !suppressOutput && bitReverse.io.o_last
      wrapperOutputValid := io.out.valid
      wrapperOutputFire  := io.out.fire
      wrapperOutputLast  := io.o_last
      coreInputLastFire  := !zeroDraining && fft.io.in.fire && io.i_last
    } else {
      bitReverse.io.in.valid := !frameDrainBlocking && !zeroDraining && io.in.valid
      bitReverse.io.in.bits  := io.in.bits
      bitReverse.io.i_last   := io.i_last
      io.in.ready            := !frameDrainBlocking && !zeroDraining && bitReverse.io.in.ready

      fft.io.in.valid        := zeroDraining || bitReverse.io.out.valid
      fft.io.in.bits         := Mux(zeroDraining, 0.U.asTypeOf(params.inDataType), bitReverse.io.out.bits)
      fft.io.i_last          := Mux(zeroDraining, false.B, bitReverse.io.o_last)
      bitReverse.io.out.ready := !zeroDraining && fft.io.in.ready

      io.out.valid := !suppressOutput && fft.io.out.valid
      fft.io.out.ready := suppressOutput || io.out.ready
      io.out.bits := fft.io.out.bits
      io.o_last := !suppressOutput && fft.io.o_last
      wrapperOutputValid := io.out.valid
      wrapperOutputFire  := io.out.fire
      wrapperOutputLast  := io.o_last
      coreInputLastFire  := !zeroDraining && bitReverse.io.out.fire && bitReverse.io.o_last
    }
  }
}
