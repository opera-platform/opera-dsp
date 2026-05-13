package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

private[cfar] class CFARStreamCore[T <: Data: Real: BinaryRepresentation](val params: CFARParams[T]) extends Module {
  CFARTypeSupport.requireSupportedParams(params)

  val io: CFARIO[T] = IO(CFARIO(params))

  private def selectRuntimeTap[A <: Data](taps: Vec[A], depth: UInt): A = {
    Mux1H((1 to taps.length).map { depthValue =>
      (depth === depthValue.U) -> taps(depthValue - 1)
    })
  }

  private def elasticPipeline[A <: Data](
    i_payload: A,
    i_valid:   Bool,
    o_ready:   Bool,
    stages:    Int
  ): (A, Bool, Bool) = {
    if (stages == 0) {
      (i_payload, i_valid, o_ready)
    } else {
      val r_pipe_payload = Reg(Vec(stages, chiselTypeOf(i_payload)))
      val r_pipe_valid = RegInit(VecInit(Seq.fill(stages)(false.B)))
      val w_pipe_en = Wire(Vec(stages, Bool()))

      for (stage <- 0 until stages) {
        when(w_pipe_en(stage)) {
          if (stage == 0) {
            r_pipe_payload(stage) := i_payload
            r_pipe_valid(stage) := i_valid
          } else {
            r_pipe_payload(stage) := r_pipe_payload(stage - 1)
            r_pipe_valid(stage) := r_pipe_valid(stage - 1)
          }
        }
      }

      for (stage <- (0 until stages).reverse) {
        if (stage == stages - 1) {
          w_pipe_en(stage) := o_ready || !r_pipe_valid(stage)
        } else {
          w_pipe_en(stage) := w_pipe_en(stage + 1) || !r_pipe_valid(stage)
        }
      }

      (r_pipe_payload.last, r_pipe_valid.last, w_pipe_en.head)
    }
  }

  private val threshold_pipe_stages =
    if (params.runtimeLogMode) params.addPipeStages.max(params.mulPipeStages)
    else if (params.logMode) params.addPipeStages
    else params.mulPipeStages
  private val retiming_stages = if (params.retiming) 1 else 0
  private val output_delay_stages = threshold_pipe_stages + retiming_stages
  private val output_queue_depth = if (params.retiming) threshold_pipe_stages + 1 else threshold_pipe_stages

  val r_in_bin = RegInit(0.U(log2Ceil(params.maxFftSize).W))
  val r_out_bin = RegInit(0.U(log2Ceil(params.maxFftSize).W))
  val r_warmup_done = RegInit(false.B)
  val r_flushing = RegInit(false.B)
  val r_cut_drain = RegInit(false.B)
  val r_pipe_draining = RegInit(false.B)

  val w_window_delay = io.i_reference_cells +& io.i_guard_cells +& 1.U

  assert(
    io.i_fft_size > 2.U * io.i_reference_cells + 2.U * io.i_guard_cells + 1.U,
    "FFT size must be larger than the active CFAR reference, guard, and CUT cells"
  )
  assert(io.i_reference_cells > 0.U, "Number of reference cells must be greater than zero")
  assert(io.i_guard_cells > 0.U, "Number of guard cells must be greater than zero")

  val sum_type = (io.i_data.bits * log2Ceil(params.maxReferenceCells)).cloneType
  val r_ref_sum_left = RegInit(0.U.asTypeOf(sum_type))
  val r_ref_sum_right = RegInit(0.U.asTypeOf(sum_type))

  val left_ref_delay =
    Module(new ReferenceDelayCells(params.inputType, params.maxReferenceCells, params.minSRAMDepth))
  left_ref_delay.io.i_data <> io.i_data
  left_ref_delay.io.i_depth := io.i_reference_cells
  left_ref_delay.io.i_last := io.i_last

  val left_guard_delay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  left_guard_delay.io.i_data <> left_ref_delay.io.o_data
  left_guard_delay.io.i_depth := io.i_guard_cells
  left_guard_delay.io.i_last := left_ref_delay.io.o_last

  val cut_delay = Module(new CFARCutDelay(params.inputType.cloneType))
  cut_delay.io.i_data <> left_guard_delay.io.o_data
  cut_delay.io.i_last := left_guard_delay.io.o_last

  private val out_queue = Module(new Queue(new CFARQueueItem(params), output_queue_depth + 1, flow = true, pipe = true))

  val right_guard_delay = Module(new DelayRegisterCells(params.inputType.cloneType, params.maxGuardCells))
  right_guard_delay.io.i_data <> cut_delay.io.o_data
  right_guard_delay.io.i_depth := io.i_guard_cells
  right_guard_delay.io.i_last := cut_delay.io.o_last

  val right_ref_delay =
    Module(new ReferenceDelayCells(params.inputType, params.maxReferenceCells, params.minSRAMDepth))
  right_ref_delay.io.i_data <> right_guard_delay.io.o_data
  right_ref_delay.io.i_depth := io.i_reference_cells
  right_ref_delay.io.i_last := right_guard_delay.io.o_last

  when(io.i_data.fire) {
    r_in_bin := r_in_bin + 1.U
  }

  when(r_in_bin === w_window_delay - 1.U && io.i_data.fire) {
    r_warmup_done := true.B
  }

  val w_raw_last = cut_delay.io.o_last

  when(out_queue.io.enq.fire && out_queue.io.enq.bits.last) {
    r_in_bin := 0.U
    r_pipe_draining := false.B
  }

  when(io.i_last && io.i_data.fire) {
    r_pipe_draining := true.B
  }

  val w_raw_out_valid = (r_warmup_done && io.i_data.fire) || r_flushing

  val w_suppress_edges = params.edgePolicy == CFAREdgePolicy.SuppressEdges
  val w_edge_span = io.i_reference_cells +& io.i_guard_cells
  val w_suppress_edge_out =
    if (w_suppress_edges) {
      r_out_bin < w_edge_span || r_out_bin >= io.i_fft_size - w_edge_span
    } else {
      false.B
    }

  val w_avg_left = BinaryRepresentation[T].shr(r_ref_sum_left, io.i_noise_div_shift)
  val w_avg_right = BinaryRepresentation[T].shr(r_ref_sum_right, io.i_noise_div_shift)
  val w_avg_go = Mux(w_avg_left > w_avg_right, w_avg_left, w_avg_right).asTypeOf(w_avg_left.cloneType)
  val w_avg_so = Mux(w_avg_left < w_avg_right, w_avg_left, w_avg_right).asTypeOf(w_avg_left.cloneType)
  val w_avg_ca = BinaryRepresentation[T].shr(w_avg_right + w_avg_left, 1).asTypeOf(w_avg_left.cloneType)

  val w_avg_mode = MuxLookup(io.i_cfar_mode, w_avg_so)(Seq(
    CFARMode.CellAveraging.U -> w_avg_ca,
    CFARMode.GreatestOf.U -> w_avg_go,
    CFARMode.SmallestOf.U -> w_avg_so
  ))
  val w_avg_zero = 0.U.asTypeOf(w_avg_mode.cloneType)

  val r_right_avg_en = RegInit(false.B)
  when(!left_ref_delay.io.o_full && left_ref_delay.io.o_data.fire) {
    r_right_avg_en := true.B
  }
  when(out_queue.io.enq.fire && out_queue.io.enq.bits.last) {
    r_right_avg_en := false.B
  }

  val w_avg_pre_scale =
    Mux(
      !right_ref_delay.io.o_full && !left_ref_delay.io.o_full,
      w_avg_zero,
      Mux(
        left_ref_delay.io.o_full && !right_ref_delay.io.o_full,
        w_avg_left,
        Mux(r_right_avg_en, w_avg_right, w_avg_mode)
      )
    )

  val w_th =
    if (params.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(numAddPipes = 0, numMulPipes = 0)) {
        val w_th_log = Wire(params.thresholdType.cloneType)
        val w_th_lin = Wire(params.thresholdType.cloneType)
        w_th_log := w_avg_pre_scale context_+ io.i_threshold_scale
        w_th_lin := w_avg_pre_scale context_* io.i_threshold_scale
        Mux(io.i_log_mode.get, w_th_log, w_th_lin)
      }
    } else if (params.logMode) {
      DspContext.withNumAddPipes(0) {
        w_avg_pre_scale context_+ io.i_threshold_scale
      }
    } else {
      DspContext.withNumMulPipes(0) {
        w_avg_pre_scale context_* io.i_threshold_scale
      }
    }

  val w_cut = cut_delay.io.o_data.bits
  val w_left_nbr = selectRuntimeTap(left_guard_delay.io.o_taps, io.i_guard_cells)
  val w_right_nbr = left_guard_delay.io.o_taps.head
  val w_local_max = w_cut > w_left_nbr && w_cut > w_right_nbr
  val w_above_th = w_cut > w_th
  val w_peak = Mux(io.i_peak_grouping, w_above_th && w_local_max, w_above_th)

  val w_raw_payload = Wire(new CFARQueueItem(params))
  w_raw_payload.peak := Mux(w_suppress_edge_out, false.B, w_peak)
  w_raw_payload.threshold := Mux(w_suppress_edge_out, 0.U.asTypeOf(w_th), w_th)
  w_raw_payload.last := w_raw_last
  w_raw_payload.fftBin := r_out_bin
  if (params.sendCut) {
    w_raw_payload.cut.get := w_cut
  }

  val (w_queue_payload, w_queue_valid, w_raw_ready) =
    elasticPipeline(w_raw_payload, w_raw_out_valid, out_queue.io.enq.ready, output_delay_stages)

  right_guard_delay.io.o_data.ready := w_raw_ready
  right_ref_delay.io.o_data.ready := w_raw_ready
  cut_delay.io.o_data.ready := Mux(right_ref_delay.io.o_full, right_ref_delay.io.i_data.ready, w_raw_ready)

  val w_raw_out_fire = w_raw_out_valid && w_raw_ready
  val w_raw_last_fire = w_raw_out_fire && w_raw_last

  when(w_raw_out_fire) {
    r_out_bin := r_out_bin + 1.U
  }

  when((r_out_bin === io.i_fft_size - 1.U && w_raw_out_fire) || w_raw_last_fire) {
    r_out_bin := 0.U
  }

  when(io.i_last && io.i_data.fire) {
    r_flushing := true.B
  }

  when(w_raw_last_fire) {
    r_flushing := false.B
  }

  when(out_queue.io.enq.fire && out_queue.io.enq.bits.last) {
    r_warmup_done := false.B
    r_cut_drain := true.B
  }

  when(right_ref_delay.io.o_empty) {
    r_cut_drain := false.B
  }

  when(w_raw_last_fire) {
    r_ref_sum_left := 0.U.asTypeOf(sum_type)
  }.elsewhen(io.i_data.fire) {
    when(left_ref_delay.io.o_full) {
      when(left_ref_delay.io.o_data.fire) {
        r_ref_sum_left := r_ref_sum_left + left_ref_delay.io.i_data.bits - left_ref_delay.io.o_data.bits
      }
    }.otherwise {
      r_ref_sum_left := r_ref_sum_left + left_ref_delay.io.i_data.bits
    }
  }

  when(r_cut_drain) {
    r_ref_sum_right := 0.U.asTypeOf(sum_type)
  }.elsewhen(right_ref_delay.io.i_data.fire) {
    when(right_ref_delay.io.o_full) {
      when(right_ref_delay.io.o_data.fire) {
        r_ref_sum_right := r_ref_sum_right + right_ref_delay.io.i_data.bits - right_ref_delay.io.o_data.bits
      }
    }.otherwise {
      r_ref_sum_right := r_ref_sum_right + right_ref_delay.io.i_data.bits
    }
  }

  val w_fill_window = !r_warmup_done && r_in_bin < w_window_delay
  io.i_data.ready := !r_pipe_draining && (w_fill_window || w_raw_ready)

  out_queue.io.enq.valid := w_queue_valid
  out_queue.io.enq.bits := w_queue_payload

  out_queue.io.deq.ready := io.o_data.ready

  io.o_data.valid := out_queue.io.deq.valid
  io.o_data.bits.peak := out_queue.io.deq.bits.peak
  io.o_data.bits.threshold := out_queue.io.deq.bits.threshold
  if (params.sendCut) {
    io.o_data.bits.cut.get := out_queue.io.deq.bits.cut.get
  }
  io.o_last := out_queue.io.deq.bits.last
  io.o_fft_bin := out_queue.io.deq.bits.fftBin
}
