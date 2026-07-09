package opera.cfar

import chisel3._
import chisel3.experimental.{requireIsChiselType, requireIsHardware}
import chisel3.util._

object DelayRegisterCells {
  def apply[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, enable: Bool = true.B): T = {
    withTaps(in, maxDepth, depth, resetData, enable)._1
  }

  // Runtime-depth register delay with every tap exposed for neighbor selection.
  def withTaps[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, enable: Bool = true.B): (T, Vec[T]) = {
    require(maxDepth > 0, s"maxDepth must be positive, got $maxDepth")
    requireIsHardware(in)
    assert(depth <= maxDepth.U)

    val r_taps = RegInit(VecInit(Seq.fill(maxDepth)(resetData)))
    for (index <- 0 until maxDepth) {
      val w_next = if (index == 0) in else r_taps(index - 1)
      when(enable && depth > index.U) {
        r_taps(index) := w_next
      }
    }

    val w_selected_tap =
      if (maxDepth == 1) {
        r_taps.head
      } else {
        Mux1H((1 to maxDepth).map { depthValue =>
          (depth === depthValue.U) -> r_taps(depthValue - 1)
        })
      }
    val w_delay_out = Mux(depth === 0.U, resetData, w_selected_tap)
    (w_delay_out, r_taps)
  }
}

class ReferenceDelayCells[T <: Data](
  val dataType: T,
  val maxDepth: Int,
  val minSRAMDepth: Int
) extends Module {
  require(minSRAMDepth >= 0, s"minSRAMDepth must be non-negative, got $minSRAMDepth")

  val io = IO(new Bundle {
    val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))
    val i_data  = Flipped(Decoupled(dataType.cloneType))
    val i_last  = Input(Bool())

    val o_data  = Decoupled(dataType.cloneType)
    val o_last  = Output(Bool())
    val o_full  = Output(Bool())
    val o_empty = Output(Bool())
  })

  // Pick a register or SRAM-backed implementation without changing the public IO.
  private val use_sram = minSRAMDepth < maxDepth
  if (use_sram) {
    val delay_core = Module(new DelaySRAMCells(dataType, maxDepth))
    delay_core.io.i_depth := io.i_depth
    delay_core.io.i_data  <> io.i_data
    delay_core.io.i_last  := io.i_last
    io.o_data  <> delay_core.io.o_data
    io.o_last  := delay_core.io.o_last
    io.o_full  := delay_core.io.o_full
    io.o_empty := delay_core.io.o_empty
  } else {
    val delay_core = Module(new DelayRegisterCells(dataType, maxDepth))
    delay_core.io.i_depth := io.i_depth
    delay_core.io.i_data  <> io.i_data
    delay_core.io.i_last  := io.i_last
    io.o_data  <> delay_core.io.o_data
    io.o_last  := delay_core.io.o_last
    io.o_full  := delay_core.io.o_full
    io.o_empty := delay_core.io.o_empty
  }
}

class DelayRegisterCells[T <: Data](val dataType: T, val maxDepth: Int) extends Module {
  require(maxDepth > 0, s"maxDepth must be positive, got $maxDepth")
  requireIsChiselType(dataType)

  val io = IO(new Bundle {
    val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))
    val i_data  = Flipped(Decoupled(dataType.cloneType))
    val i_last  = Input(Bool())

    val o_data  = Decoupled(dataType.cloneType)
    val o_last  = Output(Bool())
    val o_taps  = Output(Vec(maxDepth, dataType.cloneType))
    val o_full  = Output(Bool())
    val o_empty = Output(Bool())
  })

  // State tracks when the variable-depth window is full and when the tail is draining.
  val r_in_count  = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val r_fill_done = RegInit(false.B)
  val r_draining  = RegInit(false.B)
  val w_zero_data = 0.U.asTypeOf(io.i_data.bits)
  val w_bypass    = io.i_depth === 0.U
  val w_in_fire   = io.i_data.fire && !w_bypass
  val w_shift_en  = w_in_fire || (r_draining && io.o_data.ready)
  val w_fill_done = w_in_fire && (r_in_count === io.i_depth - 1.U)

  // Data and last travel through matching delay chains.
  val (w_delay_sample, r_taps) =
    DelayRegisterCells.withTaps(io.i_data.bits, maxDepth, io.i_depth, w_zero_data, w_shift_en)
  val w_last_in_fire = io.i_last && w_in_fire
  val r_last_out = DelayRegisterCells(w_last_in_fire, maxDepth, io.i_depth, resetData = false.B, enable = w_shift_en)

  when(w_last_in_fire) {
    r_draining := true.B
  }

  when(w_in_fire) {
    r_in_count := r_in_count + 1.U
  }

  when(w_fill_done) {
    r_fill_done := true.B
  }

  when(r_last_out && io.o_data.fire) {
    r_in_count := 0.U
    r_fill_done := false.B
    r_draining := false.B
  }

  io.o_empty      := r_in_count === 0.U && !r_fill_done
  io.o_full       := r_fill_done && !r_draining
  io.i_data.ready := Mux(w_bypass, io.o_data.ready, !r_fill_done || (io.o_data.ready && !r_draining))
  io.o_data.bits  := Mux(w_bypass, io.i_data.bits, w_delay_sample)
  io.o_data.valid := Mux(w_bypass, io.i_data.valid, (r_fill_done && io.i_data.valid) || (r_draining && w_shift_en))
  io.o_last       := Mux(w_bypass, io.i_last && io.i_data.fire, r_last_out)
  io.o_taps       := r_taps
}

class DelaySRAMCells[T <: Data](val dataType: T, val maxDepth: Int) extends Module {
  require(maxDepth > 1, s"maxDepth must be greater than 1, got $maxDepth")
  requireIsChiselType(dataType)

  val io = IO(new Bundle {
    val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))
    val i_data  = Flipped(Decoupled(dataType.cloneType))
    val i_last  = Input(Bool())

    val o_data  = Decoupled(dataType.cloneType)
    val o_last  = Output(Bool())
    val o_full  = Output(Bool())
    val o_empty = Output(Bool())
  })

  // State and the small queue hide SyncReadMem response latency.
  // TODO: Currently memories store raw bits: CIRCT cannot lower signed-typed (e.g. FixedPoint) memories. In future, we may want to support signed memories and/or use a more generic memory abstraction.
  val m_delay     = SyncReadMem(maxDepth, UInt(dataType.getWidth.W))
  val out_queue   = Module(new Queue(UInt(dataType.getWidth.W), 2, pipe = true, flow = true))
  val r_wr_idx    = RegInit(0.U(log2Ceil(maxDepth).W))
  val r_in_count  = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val r_fill_done = RegInit(false.B)
  val r_draining  = RegInit(false.B)
  val w_zero_data = 0.U.asTypeOf(io.i_data.bits)
  val w_bypass    = io.i_depth === 0.U

  val w_out_fire  = io.o_data.fire && !w_bypass
  val w_out_ready = io.o_data.ready && out_queue.io.enq.ready && !w_bypass
  val w_in_fire   = io.i_data.fire && !w_bypass
  val w_fill_done = w_in_fire && (r_in_count === io.i_depth - 1.U)

  io.i_data.ready := Mux(w_bypass, io.o_data.ready, !r_fill_done || (w_out_ready && !r_draining))

  // Memory advances on accepted inputs, then on tail-drain cycles after i_last.
  val w_last_in_fire = io.i_last && w_in_fire
  val r_last_out     = DelayRegisterCells(w_last_in_fire, maxDepth, io.i_depth, resetData = false.B, enable = w_out_fire)
  val w_reset_all    = r_last_out && w_out_fire
  val w_drain_adv    = r_draining && w_out_ready && out_queue.io.deq.valid && !w_reset_all
  val w_adv          = w_in_fire || w_drain_adv

  // Read the sample that is `i_depth` entries behind the next write position.
  val w_next_wr_idx = Mux(r_wr_idx === (maxDepth - 1).U, 0.U, r_wr_idx + 1.U)(log2Ceil(maxDepth) - 1, 0)
  val w_rd_addr = Mux(
    w_next_wr_idx >= io.i_depth,
    w_next_wr_idx - io.i_depth,
    maxDepth.U + w_next_wr_idx - io.i_depth
  )(log2Ceil(maxDepth) - 1, 0)
  val w_rd_en = w_adv && io.i_depth > 1.U
  val w_mem_data = m_delay.read(w_rd_addr, w_rd_en)
  val w_direct_data = Mux(w_in_fire, io.i_data.bits, w_zero_data)
  val r_resp_direct = RegEnable(io.i_depth <= 1.U, false.B, w_adv)
  val r_direct_data = RegEnable(w_direct_data, w_zero_data, w_adv)
  val r_resp_valid = RegNext(
    w_fill_done || (r_fill_done && w_in_fire) || w_drain_adv,
    init = false.B
  )

  when(w_adv && !w_reset_all) {
    m_delay.write(r_wr_idx, w_direct_data.asUInt)
    r_wr_idx := w_next_wr_idx
  }

  when(w_in_fire) {
    r_in_count := r_in_count + 1.U
  }

  when(w_fill_done) {
    r_fill_done := true.B
  }

  when(w_last_in_fire) {
    r_draining := true.B
  }

  when(w_reset_all) {
    r_wr_idx := 0.U
    r_in_count := 0.U
    r_fill_done := false.B
    r_draining := false.B
  }

  out_queue.io.enq.valid := r_resp_valid
  out_queue.io.enq.bits  := Mux(r_resp_direct, r_direct_data.asUInt, w_mem_data)
  out_queue.io.deq.ready := io.o_data.ready && !w_bypass

  io.o_data.bits  := Mux(w_bypass, io.i_data.bits, out_queue.io.deq.bits.asTypeOf(io.o_data.bits))
  io.o_data.valid := Mux(w_bypass, io.i_data.valid, out_queue.io.deq.valid)
  io.o_last       := Mux(w_bypass, io.i_last && io.i_data.fire, r_last_out)
  io.o_full       := r_fill_done && !r_draining
  io.o_empty      := r_in_count === 0.U && !r_fill_done
}

class CFARCutDelay[T <: Data](val dataType: T) extends Module {
  requireIsChiselType(dataType)

  val io = IO(new Bundle {
    val i_data = Flipped(Decoupled(dataType.cloneType))
    val i_last = Input(Bool())
    val o_data = Decoupled(dataType.cloneType)
    val o_last = Output(Bool())
  })

  // One-sample CUT delay aligns the cell under test between the guard delays.
  val r_fill_done = RegInit(false.B)
  val r_draining  = RegInit(false.B)
  val w_zero_data = 0.U.asTypeOf(io.i_data.bits)
  val w_en        = io.i_data.fire || (r_draining && io.o_data.ready)
  val r_cut       = RegEnable(io.i_data.bits, w_zero_data, w_en)

  when(io.i_data.fire && !r_fill_done) {
    r_fill_done := true.B
  }

  when(io.i_last && io.i_data.fire) {
    r_draining := true.B
  }

  val w_last_in_fire = io.i_last && io.i_data.fire
  val r_last_out = RegEnable(w_last_in_fire, false.B, io.o_data.ready)

  when(r_last_out && io.o_data.ready) {
    r_fill_done := false.B
    r_draining  := false.B
  }

  io.i_data.ready := !r_fill_done || io.o_data.ready && !r_draining
  io.o_data.bits  := r_cut
  io.o_data.valid := r_fill_done && io.i_data.valid || (r_draining && w_en)
  io.o_last       := r_last_out
}
