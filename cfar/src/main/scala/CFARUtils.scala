package opera.cfar

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import fixedpoint._

private[cfar] class CFARRuntimeConfig[T <: Data: Real](val params: CFARParams[T]) extends Bundle {
  val fft_size         = UInt(log2Ceil(params.maxFftSize + 1).W)
  val reference_cells  = UInt(log2Ceil(params.maxReferenceCells + 1).W)
  val guard_cells      = UInt(log2Ceil(params.maxGuardCells + 1).W)
  val noise_div_shift  = UInt(CFARRuntimeConfig.noiseShiftWidth(params).W)
  val order_rank_left  = UInt(log2Ceil(params.maxReferenceCells + 1).W)
  val order_rank_right = UInt(log2Ceil(params.maxReferenceCells + 1).W)
  val cfar_mode        = UInt(2.W)
  val edge_policy      = UInt(2.W)
  val peak_grouping    = Bool()
  val threshold_scale  = params.scaleType.cloneType
  val log_mode         = Bool()
}

private[cfar] object CFARRuntimeConfig {
  def noiseShiftWidth[T <: Data: Real](params: CFARParams[T]): Int =
    math.max(1, log2Ceil(log2Ceil(params.maxReferenceCells + 1)))

  def default[T <: Data: Real](params: CFARParams[T]): CFARRuntimeConfig[T] =
    fromFields(
      params,
      fftSize        = params.maxFftSize.U,
      referenceCells = params.maxReferenceCells.U,
      guardCells     = params.maxGuardCells.U,
      noiseDivShift  = log2Ceil(params.maxReferenceCells).U(noiseShiftWidth(params).W),
      orderRankLeft  = 1.U,
      orderRankRight = 1.U,
      cfarMode       = CFARMode.CellAveraging.U,
      edgePolicy     = params.edgePolicy.U,
      peakGrouping   = false.B,
      thresholdScale = 0.U.asTypeOf(params.scaleType),
      logMode        = params.logMode.B
    )

  def fromIo[T <: Data: Real](params: CFARParams[T], io: CFARIO[T]): CFARRuntimeConfig[T] =
    fromFields(
      params,
      fftSize        = io.i_fft_size,
      referenceCells = io.i_reference_cells,
      guardCells     = io.i_guard_cells,
      noiseDivShift  = io.i_noise_div_shift.getOrElse(log2Ceil(params.maxReferenceCells).U(noiseShiftWidth(params).W)),
      orderRankLeft  = io.i_order_rank_left.getOrElse(1.U),
      orderRankRight = io.i_order_rank_right.getOrElse(1.U),
      cfarMode       = io.i_cfar_mode,
      edgePolicy     = io.i_edge_policy.getOrElse(params.edgePolicy.U(2.W)),
      peakGrouping   = io.i_peak_grouping,
      thresholdScale = io.i_threshold_scale,
      logMode        = io.i_log_mode.getOrElse(params.logMode.B)
    )

  def fromFields[T <: Data: Real](
    params        : CFARParams[T],
    fftSize       : UInt,
    referenceCells: UInt,
    guardCells    : UInt,
    noiseDivShift : UInt,
    orderRankLeft : UInt,
    orderRankRight: UInt,
    cfarMode      : UInt,
    edgePolicy    : UInt,
    peakGrouping  : Bool,
    thresholdScale: T,
    logMode       : Bool
  ): CFARRuntimeConfig[T] = {
    val w_cfg = Wire(new CFARRuntimeConfig(params))
    w_cfg.fft_size         := fftSize
    w_cfg.reference_cells  := referenceCells
    w_cfg.guard_cells      := guardCells
    w_cfg.noise_div_shift  := noiseDivShift
    w_cfg.order_rank_left  := orderRankLeft
    w_cfg.order_rank_right := orderRankRight
    w_cfg.cfar_mode        := cfarMode
    w_cfg.edge_policy      := edgePolicy
    w_cfg.peak_grouping    := peakGrouping
    w_cfg.threshold_scale  := thresholdScale
    w_cfg.log_mode         := logMode
    w_cfg
  }
}

private[cfar] final case class CFAROutputQueueIO[T <: Data: Real](
  deq        : DecoupledIO[CFARQueuePayload[T]],
  inputReady : Bool,
  enqLastFire: Bool,
  deqLastFire: Bool
)

private[cfar] object CFARUtils {
  def widenedSumType[T <: Data](inputType: T, maxTerms: Int): T = {
    require(maxTerms > 0, "maxTerms must be positive")
    val growth = log2Ceil(maxTerms)
    inputType match {
      case _: UInt => UInt((inputType.getWidth + growth).W).asInstanceOf[T]
      case _: SInt => SInt((inputType.getWidth + growth).W).asInstanceOf[T]
      case fixed: FixedPoint =>
        require(fixed.binaryPoint.known, "FixedPoint sum type requires a known binary point")
        FixedPoint((fixed.getWidth + growth).W, fixed.binaryPoint.get.BP).asInstanceOf[T]
      case other =>
        throw new IllegalArgumentException(s"Unsupported CFAR sum type: ${other.getClass.getSimpleName}")
    }
  }

  def thresholdPipeStages[T <: Data: Real](params: CFARParams[T]): Int =
    if (params.runtimeLogMode) params.addPipeStages.max(params.mulPipeStages)
    else if (params.logMode) params.addPipeStages
    else params.mulPipeStages

  def outputDelayStages[T <: Data: Real](params: CFARParams[T]): Int =
    thresholdPipeStages(params) + (if (params.retiming) 1 else 0)

  def outputQueueDepth[T <: Data: Real](params: CFARParams[T]): Int =
    if (params.retiming) thresholdPipeStages(params) + 1 else thresholdPipeStages(params)

  def selectRuntimeValue[A <: Data](values: Vec[A], oneBasedIndex: UInt): A = {
    require(values.length > 0, "values must be non-empty")
    if (values.length == 1) {
      values.head
    } else {
      Mux1H((1 to values.length).map { index =>
        (oneBasedIndex === index.U) -> values(index - 1)
      })
    }
  }

  def greaterThan[T <: Data](left: T, right: T): Bool = (left, right) match {
    case (l: UInt, r: UInt)             => l > r
    case (l: SInt, r: SInt)             => l > r
    case (l: FixedPoint, r: FixedPoint) => l > r
    case _ => throw new IllegalArgumentException("Unsupported CFAR compare type")
  }

  def lessThan[T <: Data](left: T, right: T): Bool = (left, right) match {
    case (l: UInt, r: UInt)             => l < r
    case (l: SInt, r: SInt)             => l < r
    case (l: FixedPoint, r: FixedPoint) => l < r
    case _ => throw new IllegalArgumentException("Unsupported CFAR compare type")
  }

  def maxOf[T <: Data](left: T, right: T): T =
    Mux(greaterThan(left, right), left, right).asTypeOf(left.cloneType)

  def minOf[T <: Data](left: T, right: T): T =
    Mux(lessThan(left, right), left, right).asTypeOf(left.cloneType)

  def averagePair[T <: Data: BinaryRepresentation](left: T, right: T): T = {
    val w_avg = Wire(left.cloneType)
    (left, right) match {
      case (l: UInt, r: UInt) => w_avg := ((l +& r) >> 1).asTypeOf(left)
      case (l: SInt, r: SInt) => w_avg := ((l +& r) >> 1).asTypeOf(left)
      case (l: FixedPoint, r: FixedPoint) =>  w_avg := ((l.asSInt +& r.asSInt) >> 1).asTypeOf(left)
      case _ => throw new IllegalArgumentException("Unsupported CFAR average type")
    }
    w_avg
  }

  def elasticPipeline[A <: Data](
    i_payload: A,
    i_valid  : Bool,
    o_ready  : Bool,
    stages   : Int
  ): (A, Bool, Bool) = {
    if (stages == 0) {
      (i_payload, i_valid, o_ready)
    } else {
      val r_pipe_payload = Reg(Vec(stages, chiselTypeOf(i_payload)))
      val r_pipe_valid   = RegInit(VecInit(Seq.fill(stages)(false.B)))
      val w_pipe_en      = Wire(Vec(stages, Bool()))

      for (stage <- 0 until stages) {
        when(w_pipe_en(stage)) {
          if (stage == 0) {
            r_pipe_payload(stage) := i_payload
            r_pipe_valid(stage)   := i_valid
          } else {
            r_pipe_payload(stage) := r_pipe_payload(stage - 1)
            r_pipe_valid(stage)   := r_pipe_valid(stage - 1)
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

  def resultPayload[T <: Data: Real](
    params   : CFARParams[T],
    peak     : Bool,
    threshold: T,
    cut      : T,
    last     : Bool,
    fftBin   : UInt,
    suppress : Bool = false.B
  ): CFARQueuePayload[T] = {
    val w_payload = Wire(new CFARQueuePayload(params))
    w_payload.output.peak      := Mux(suppress, false.B, peak)
    w_payload.output.threshold := Mux(suppress, 0.U.asTypeOf(threshold), threshold)
    w_payload.last             := last
    w_payload.fftBin           := fftBin
    if (params.sendCut) {
      w_payload.output.cut.get := cut
    }
    w_payload
  }

  def outputQueue[T <: Data: Real](
    params      : CFARParams[T],
    rawPayload  : CFARQueuePayload[T],
    rawValid    : Bool,
    outputReady : Bool,
    delayStages : Int,
    queueDepth  : Int
  ): CFAROutputQueueIO[T] = {
    val out_queue = Module(new Queue(new CFARQueuePayload(params), queueDepth + 1, flow = true, pipe = true))
    val (w_queue_payload, w_queue_valid, w_input_ready) =
      elasticPipeline(rawPayload, rawValid, out_queue.io.enq.ready, delayStages)

    out_queue.io.enq.valid := w_queue_valid
    out_queue.io.enq.bits  := w_queue_payload
    out_queue.io.deq.ready := outputReady

    CFAROutputQueueIO(
      deq         = out_queue.io.deq,
      inputReady  = w_input_ready,
      enqLastFire = out_queue.io.enq.fire && out_queue.io.enq.bits.last,
      deqLastFire = out_queue.io.deq.fire && out_queue.io.deq.bits.last
    )
  }

  def connectRuntimeEdgeRouter[T <: Data: Real](
    params  : CFARParams[T],
    top     : CFARIO[T],
    streamIo: CFARIO[T],
    cyclicIo: CFARIO[T]
  ): Unit = {
    def connectInputs(dst: CFARIO[T], valid: Bool): Unit = {
      dst.i_data.bits       := top.i_data.bits
      dst.i_data.valid      := top.i_data.valid && valid
      dst.i_last            := top.i_last
      dst.i_load_cfg        := top.i_load_cfg
      dst.i_fft_size        := top.i_fft_size
      dst.i_threshold_scale := top.i_threshold_scale
      dst.i_peak_grouping   := top.i_peak_grouping
      dst.i_cfar_mode       := top.i_cfar_mode
      dst.i_reference_cells := top.i_reference_cells
      dst.i_guard_cells     := top.i_guard_cells
      dst.i_log_mode.foreach(_ := top.i_log_mode.get)
      dst.i_edge_policy.foreach(_ := top.i_edge_policy.get)
      dst.i_noise_div_shift.foreach(_ := top.i_noise_div_shift.get)
      dst.i_order_rank_left.foreach(_ := top.i_order_rank_left.get)
      dst.i_order_rank_right.foreach(_ := top.i_order_rank_right.get)
    }

    val r_pending_edge = RegInit(params.edgePolicy.U(2.W))
    when(top.i_load_cfg) {
      r_pending_edge := top.i_edge_policy.get
    }

    val w_next_edge  = Mux(top.i_load_cfg, top.i_edge_policy.get, r_pending_edge)
    val w_next_wrap  = w_next_edge === CFAREdgePolicy.WrapAroundFrame.U
    val r_active     = RegInit(false.B)
    val r_route_wrap = RegInit(false.B)
    val w_route_wrap = Mux(r_active, r_route_wrap, w_next_wrap)

    connectInputs(streamIo, !w_route_wrap)
    connectInputs(cyclicIo, w_route_wrap)

    top.i_data.ready := Mux(w_route_wrap, cyclicIo.i_data.ready, streamIo.i_data.ready)

    cyclicIo.o_data.ready := Mux(r_active && r_route_wrap, top.o_data.ready, false.B)
    streamIo.o_data.ready := Mux(r_active && !r_route_wrap, top.o_data.ready, false.B)

    val w_use_wrap_output = r_active && r_route_wrap
    top.o_data.valid := Mux(w_use_wrap_output, cyclicIo.o_data.valid, streamIo.o_data.valid)
    top.o_data.bits  := Mux(w_use_wrap_output, cyclicIo.o_data.bits, streamIo.o_data.bits)
    top.o_last       := Mux(w_use_wrap_output, cyclicIo.o_last, streamIo.o_last)
    top.o_fft_bin    := Mux(w_use_wrap_output, cyclicIo.o_fft_bin, streamIo.o_fft_bin)

    val w_wrap_done   = cyclicIo.o_data.fire && cyclicIo.o_last
    val w_stream_done = streamIo.o_data.fire && streamIo.o_last
    when(top.i_data.fire && !r_active) {
      r_active := true.B
      r_route_wrap := w_next_wrap
    }
    when(r_active && Mux(r_route_wrap, w_wrap_done, w_stream_done)) {
      r_active := false.B
    }
  }

  def caModeAverage[T <: Data: BinaryRepresentation](
    leftAverage : T,
    rightAverage: T,
    cfarMode    : UInt
  ): T = {
    val w_ca = averagePair(leftAverage, rightAverage)
    val w_go = maxOf(leftAverage, rightAverage)
    val w_so = minOf(leftAverage, rightAverage)

    MuxLookup(cfarMode, w_so)(Seq(
      CFARMode.CellAveraging.U -> w_ca,
      CFARMode.GreatestOf.U    -> w_go,
      CFARMode.SmallestOf.U    -> w_so
    )).asTypeOf(leftAverage.cloneType)
  }

  def selectNonWrapEdgeNoise[T <: Data](
    leftNoise        : T,
    rightNoise       : T,
    bothSidesNoise   : T,
    edgePolicy       : UInt,
    runtimeEdgePolicy: Boolean,
    staticEdgePolicy : Int,
    isLeftEdge       : Bool,
    isRightEdge      : Bool
  ): (T, Bool) = {
    val w_edge          = isLeftEdge || isRightEdge
    val w_zero          = 0.U.asTypeOf(bothSidesNoise)
    val w_suppress_edge =
      if (runtimeEdgePolicy) {
        edgePolicy === CFAREdgePolicy.SuppressEdges.U && w_edge
      } else if (staticEdgePolicy == CFAREdgePolicy.SuppressEdges) {
        w_edge
      } else {
        false.B
      }
    val w_one_sided = Mux(
      isLeftEdge && !isRightEdge,
      rightNoise,
      Mux(isRightEdge && !isLeftEdge, leftNoise, Mux(w_edge, w_zero, bothSidesNoise))
    ).asTypeOf(bothSidesNoise.cloneType)
    val w_suppressed = Mux(w_edge, w_zero, bothSidesNoise).asTypeOf(bothSidesNoise.cloneType)
    val w_noise =
      if (runtimeEdgePolicy) {
        MuxLookup(edgePolicy, w_one_sided)(Seq(
          CFAREdgePolicy.SuppressEdges.U   -> w_suppressed,
          CFAREdgePolicy.OneSidedAverage.U -> w_one_sided,
          CFAREdgePolicy.WrapAroundFrame.U -> bothSidesNoise
        ))
      } else {
        staticEdgePolicy match {
          case CFAREdgePolicy.SuppressEdges   => w_suppressed
          case CFAREdgePolicy.OneSidedAverage => w_one_sided
          case CFAREdgePolicy.WrapAroundFrame => bothSidesNoise
        }
      }

    (w_noise.asTypeOf(bothSidesNoise.cloneType), w_suppress_edge)
  }

  def linearLocalMax[T <: Data](cut: T, prev: T, next: T, fftBin: UInt, fftSize: UInt): Bool =
    (fftBin === 0.U || greaterThan(cut, prev)) &&
      (fftBin === fftSize - 1.U || greaterThan(cut, next))

  def thresholdScale[T <: Data: Real](
    params        : CFARParams[T],
    noiseEstimate : T,
    thresholdScale: T,
    logMode       : Bool
  ): T = {
    val w_threshold = Wire(params.thresholdType.cloneType)
    if (params.runtimeLogMode) {
      DspContext.alter(DspContext.current.copy(numAddPipes = 0, numMulPipes = 0)) {
        val w_threshold_log = Wire(params.thresholdType.cloneType)
        val w_threshold_lin = Wire(params.thresholdType.cloneType)
        w_threshold_log := noiseEstimate context_+ thresholdScale
        w_threshold_lin := noiseEstimate context_* thresholdScale
        w_threshold     := Mux(logMode, w_threshold_log, w_threshold_lin)
      }
    } else if (params.logMode) {
      DspContext.withNumAddPipes(0) {
        w_threshold := noiseEstimate context_+ thresholdScale
      }
    } else {
      DspContext.withNumMulPipes(0) {
        w_threshold := noiseEstimate context_* thresholdScale
      }
    }
    w_threshold
  }
}
