package opera.cfar

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util._
import dsptools.numbers.Real

private[cfar] object CFARFrameReplay {
  def internal_max_fft_size[T <: Data: Real](params: CFARParams[T]): Int = {
    val max_replay_frame_samples = params.maxFftSize + 2 * (params.maxReferenceCells + params.maxGuardCells)
    1 << log2Ceil(max_replay_frame_samples)
  }
}

private[cfar] class CFARFrameReplay[T <: Data: Real](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)
  requireIsChiselType(params.inputType)

  // Parameter widths and config defaults
  private val internal_max_fft_size = CFARFrameReplay.internal_max_fft_size(params)
  private val frame_idx_width       = log2Ceil(params.maxFftSize)
  private val edge_span_width       = log2Ceil(params.maxReferenceCells + params.maxGuardCells + 1)
  private val replay_idx_width      = log2Ceil(internal_max_fft_size)

  val io = IO(new Bundle {
    val i_data     = Flipped(Decoupled(params.inputType))
    val i_last     = Input(Bool())
    val i_load_cfg = Input(Bool())

    val i_fft_size         = Input(UInt(log2Ceil(params.maxFftSize + 1).W))
    val i_threshold_scale  = Input(params.scaleType)
    val i_log_mode         = Input(Bool())
    val i_peak_grouping    = Input(Bool())
    val i_cfar_mode        = Input(UInt(2.W))
    val i_edge_policy      = Input(UInt(2.W))
    val i_reference_cells  = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
    val i_guard_cells      = Input(UInt(log2Ceil(params.maxGuardCells + 1).W))
    val i_noise_div_shift  = Input(UInt(CFARRuntimeConfig.noiseShiftWidth(params).W))
    val i_order_rank_left  = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))
    val i_order_rank_right = Input(UInt(log2Ceil(params.maxReferenceCells + 1).W))

    val o_data                 = Decoupled(params.inputType)
    val o_last                 = Output(Bool())
    val o_cfg         = Output(new CFARRuntimeConfig(params))
    val i_output_done = Input(Bool())
  })

  private def default_config: CFARRuntimeConfig[T] = CFARRuntimeConfig.default(params)

  // IO config capture
  private val w_input_cfg = CFARRuntimeConfig.fromFields(
    params,
    fftSize        = io.i_fft_size,
    referenceCells = io.i_reference_cells,
    guardCells     = io.i_guard_cells,
    noiseDivShift  = io.i_noise_div_shift,
    orderRankLeft  = io.i_order_rank_left,
    orderRankRight = io.i_order_rank_right,
    cfarMode       = io.i_cfar_mode,
    edgePolicy     = io.i_edge_policy,
    peakGrouping   = io.i_peak_grouping,
    thresholdScale = io.i_threshold_scale,
    logMode        = io.i_log_mode
  )

  private val r_pending_cfg = RegInit(default_config)
  private val w_next_frame_cfg = Mux(io.i_load_cfg, w_input_cfg, r_pending_cfg)
  when(io.i_load_cfg) {
    r_pending_cfg := w_input_cfg
  }

  assert(w_next_frame_cfg.reference_cells > 0.U                          , "i_reference_cells must be greater than zero")
  assert(w_next_frame_cfg.guard_cells > 0.U                              , "i_guard_cells must be greater than zero")
  assert(w_next_frame_cfg.reference_cells <= params.maxReferenceCells.U  , "i_reference_cells exceeds maxReferenceCells")
  assert(w_next_frame_cfg.guard_cells <= params.maxGuardCells.U          , "i_guard_cells exceeds maxGuardCells")
  assert(w_next_frame_cfg.fft_size > 0.U                                 , "i_fft_size must be greater than zero")
  assert(w_next_frame_cfg.fft_size <= params.maxFftSize.U                , "i_fft_size exceeds maxFftSize")
  assert(w_next_frame_cfg.edge_policy <= CFAREdgePolicy.WrapAroundFrame.U, "i_edge_policy must be a supported CFAREdgePolicy value")
  assert(w_next_frame_cfg.order_rank_left > 0.U                          , "i_order_rank_left must be at least 1")
  assert(w_next_frame_cfg.order_rank_left <= w_next_frame_cfg.reference_cells, "i_order_rank_left exceeds i_reference_cells")
  assert(w_next_frame_cfg.order_rank_right > 0.U                         , "i_order_rank_right must be at least 1")
  assert(w_next_frame_cfg.order_rank_right <= w_next_frame_cfg.reference_cells, "i_order_rank_right exceeds i_reference_cells")
  assert(
    w_next_frame_cfg.fft_size > 2.U * w_next_frame_cfg.reference_cells + 2.U * w_next_frame_cfg.guard_cells + 1.U,
    "FFT size must be larger than the active CFAR reference, guard, and CUT cells"
  )

  // Ping-pong frame write path
  // TODO: Currently memories store raw bits: CIRCT cannot lower signed-typed (e.g. FixedPoint) memories. In future, we may want to support signed memories and/or use a more generic memory abstraction.
  private val m_frame_0            = SyncReadMem(params.maxFftSize, UInt(params.inputType.getWidth.W))
  private val m_frame_1            = SyncReadMem(params.maxFftSize, UInt(params.inputType.getWidth.W))
  private val r_frame_bank_full    = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val r_frame_write_active = RegInit(false.B)
  private val r_write_bank_sel     = RegInit(false.B)
  private val r_write_bin_idx      = RegInit(0.U(frame_idx_width.W))

  private val r_frame_bank_cfg      = RegInit(VecInit(Seq.fill(2)(default_config)))
  private val r_frame_replay_active = RegInit(false.B)
  private val r_output_done_pending = RegInit(false.B)
  private val r_replay_bank_sel     = RegInit(false.B)
  private val r_replay_sample_idx   = RegInit(0.U(replay_idx_width.W))

  private val w_frame_bank_free = VecInit(Seq(
    !r_frame_bank_full(0) && !(r_frame_replay_active && !r_replay_bank_sel) && !(r_output_done_pending && !r_replay_bank_sel),
    !r_frame_bank_full(1) && !(r_frame_replay_active && r_replay_bank_sel) && !(r_output_done_pending && r_replay_bank_sel)
  ))
  private val w_write_bank_sel  = Mux(r_frame_write_active, r_write_bank_sel, Mux(w_frame_bank_free(0), false.B, true.B))
  private val w_write_bank_free = w_frame_bank_free(w_write_bank_sel)
  io.i_data.ready := Mux(r_frame_write_active, w_write_bank_free, w_frame_bank_free.asUInt.orR)

  private val w_write_fft_size   = Mux(r_frame_write_active, r_frame_bank_cfg(r_write_bank_sel).fft_size, w_next_frame_cfg.fft_size)
  private val w_write_frame_last = r_write_bin_idx === w_write_fft_size - 1.U

  when(io.i_data.fire) {
    assert(io.i_last === w_write_frame_last, "Frame CFAR requires i_last exactly at i_fft_size - 1")

    when(w_write_bank_sel) {
      m_frame_1.write(r_write_bin_idx, io.i_data.bits.asUInt)
    }.otherwise {
      m_frame_0.write(r_write_bin_idx, io.i_data.bits.asUInt)
    }

    when(!r_frame_write_active) {
      r_write_bank_sel := w_write_bank_sel
      r_frame_bank_cfg(w_write_bank_sel) := w_next_frame_cfg
    }

    when(w_write_frame_last) {
      r_frame_bank_full(w_write_bank_sel) := true.B
      r_frame_write_active := false.B
      r_write_bin_idx := 0.U
    }.otherwise {
      r_frame_write_active := true.B
      r_write_bin_idx := r_write_bin_idx + 1.U
    }
  }

  // Replay-bank arbitration and release
  when(!r_frame_replay_active && !r_output_done_pending && r_frame_bank_full.asUInt.orR) {
    r_replay_bank_sel := !r_frame_bank_full(0)
    r_frame_replay_active := true.B
    r_replay_sample_idx := 0.U
  }

  when(r_output_done_pending && io.i_output_done) {
    r_frame_bank_full(r_replay_bank_sel) := false.B
    r_output_done_pending := false.B
  }

  // Replay address generation
  private val w_active_cfg = r_frame_bank_cfg(r_replay_bank_sel)
  private val w_wrap       = w_active_cfg.edge_policy === CFAREdgePolicy.WrapAroundFrame.U
  private val w_edge_span  = Mux(
    w_wrap,
    (w_active_cfg.reference_cells +& w_active_cfg.guard_cells)(edge_span_width - 1, 0),
    0.U(edge_span_width.W)
  )
  private val w_internal_fft_size   = w_active_cfg.fft_size.zext.asUInt +& (w_edge_span.zext.asUInt << 1)
  private val w_replay_last_sample  = r_replay_sample_idx === w_internal_fft_size - 1.U
  private val w_replay_tail_region  = w_wrap && r_replay_sample_idx < w_edge_span
  private val w_replay_frame_region = r_replay_sample_idx < (w_edge_span +& w_active_cfg.fft_size)(replay_idx_width - 1, 0)
  private val w_tail_wrap_addr      = (w_active_cfg.fft_size - w_edge_span + r_replay_sample_idx)(frame_idx_width - 1, 0)
  private val w_frame_addr          = (r_replay_sample_idx - w_edge_span)(frame_idx_width - 1, 0)
  private val w_head_wrap_addr      = (r_replay_sample_idx - w_edge_span - w_active_cfg.fft_size)(frame_idx_width - 1, 0)
  private val w_replay_read_addr    =  Mux(w_replay_tail_region, w_tail_wrap_addr, Mux(w_replay_frame_region, w_frame_addr, w_head_wrap_addr))

  // Memory response pipeline
  private val r_output_valid      = RegInit(false.B)
  private val r_output_data       = Reg(params.inputType.cloneType)
  private val r_output_last       = RegInit(false.B)
  private val r_mem_read_pending  = RegInit(false.B)
  private val w_output_fire       = r_output_valid && io.o_data.ready
  private val w_replay_read_fire  = r_frame_replay_active && !r_mem_read_pending && (!r_output_valid || w_output_fire)
  private val w_mem_data_0        = m_frame_0.read(w_replay_read_addr, w_replay_read_fire && !r_replay_bank_sel)
  private val w_mem_data_1        = m_frame_1.read(w_replay_read_addr, w_replay_read_fire && r_replay_bank_sel)
  private val r_mem_read_bank_sel = RegEnable(r_replay_bank_sel, false.B, w_replay_read_fire)
  private val r_mem_read_last     = RegEnable(w_replay_last_sample, false.B, w_replay_read_fire)

  when(w_replay_read_fire) {
    when(w_replay_last_sample) {
      r_frame_replay_active := false.B
      r_output_done_pending := true.B
      r_replay_sample_idx   := 0.U
    }.otherwise {
      r_replay_sample_idx := r_replay_sample_idx + 1.U
    }
    r_mem_read_pending := true.B
  }

  when(w_output_fire) {
    r_output_valid := false.B
  }
  when(r_mem_read_pending) {
    r_output_valid     := true.B
    r_output_data      := Mux(r_mem_read_bank_sel, w_mem_data_1, w_mem_data_0).asTypeOf(r_output_data)
    r_output_last      := r_mem_read_last
    r_mem_read_pending := false.B
  }

  // Output wiring
  io.o_data.valid := r_output_valid
  io.o_data.bits  := r_output_data
  io.o_last       := r_output_last
  io.o_cfg        := w_active_cfg
}
