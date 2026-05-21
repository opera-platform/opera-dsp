package opera.fft

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import breeze.numerics.pow
import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import dspblocks._
import dsptools.numbers.DspComplex
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

case class BitReverseParams(
  dataType     : DspComplex[FixedPoint],
  memDepth     : Int,
  runTime      : Boolean = false,
  singlePortMem: Boolean = false
) {
  requireIsChiselType(dataType)
}

class BitReverseIO(val params: BitReverseParams) extends Bundle {
  val in: DecoupledIO[DspComplex[FixedPoint]] = Flipped(Decoupled(params.dataType))
  val out: DecoupledIO[DspComplex[FixedPoint]] = Decoupled(params.dataType)
  val i_samples: Option[UInt] = if (params.runTime) Some(Input(UInt(log2Ceil(params.memDepth + 1).W))) else None

  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
}

class BitReverse(val params: BitReverseParams) extends Module {
  val io: BitReverseIO = IO(new BitReverseIO(params))

  val memDepth: Int        = params.memDepth
  private val memories     = Seq.fill(2) { SyncReadMem(memDepth, params.dataType) }
  private val w_mem_data_1 = Wire(params.dataType)
  private val w_mem_data_2 = Wire(params.dataType)
  
  // FSM
  private object FSM extends ChiselEnum { val s_idle, s_write, s_write_read, s_read = Value }
  private val r_state = RegInit(FSM.s_idle)

  private val w_valid      = r_state === FSM.s_write_read || r_state === FSM.s_read
  private val w_wr_cnt_rst = Wire(Bool())
  private val w_rd_cnt_rst = Wire(Bool())
  private val r_wr_cnt     = CounterWithReset(io.in.fire, memDepth, w_wr_cnt_rst)._1
  private val r_rd_cnt     = CounterWithReset(w_valid && io.out.ready, memDepth, w_rd_cnt_rst)._1
  private val r_wr_mem_sel: Bool = RegInit(false.B) // select in which memory to write
  private val r_rd_mem_sel: Bool = RegInit(false.B) // select from which memory to read
  private val w_sample_last = io.i_samples.getOrElse(memDepth.U) - 1.U
  private val w_wr_wrap    = r_wr_cnt === w_sample_last && io.in.fire
  private val w_rd_wrap    = r_rd_cnt === w_sample_last && io.out.fire
  private val w_out        = Cat(RegNext(w_rd_wrap), Mux(RegNext(r_rd_mem_sel), w_mem_data_2, w_mem_data_1).asUInt)
  w_wr_cnt_rst := w_wr_wrap || reset.asBool
  w_rd_cnt_rst := w_rd_wrap || reset.asBool

  // Generate write and read address
  private val fftStageNumber = log2Ceil(memDepth)
  private val fftSizes = (1 to fftStageNumber).map(n => pow(2, n).U)
  private val w_mux_case_map = fftSizes.map(
    m => m === io.i_samples.getOrElse(memDepth.U)
  ).zip(1 to fftStageNumber).map {
    case (condition, numBits) =>
      condition -> Reverse(r_wr_cnt(numBits - 1, 0))
  }
  private val w_wr_addr = MuxCase(0.U(fftStageNumber.W), w_mux_case_map)

  when(r_state === FSM.s_idle) {
    // Reset state
    r_state := FSM.s_write
  }.elsewhen(r_state === FSM.s_write) {
    // Memories are empty, we can only write to them
    when (w_wr_wrap) {
      r_state := FSM.s_write_read
    }
  }.elsewhen(r_state === FSM.s_read) {
    // Memories are full, we can only read from them
    when(w_rd_wrap) {
      r_state := FSM.s_write_read
    }
  }.elsewhen(r_state === FSM.s_write_read) {
    // We can read from one memory and write to another
    when(w_rd_wrap && !w_wr_wrap) {
      r_state := FSM.s_write
    }.elsewhen(!w_rd_wrap && w_wr_wrap) {
      r_state := FSM.s_read
    }
  }
  when (w_rd_wrap) { r_rd_mem_sel := !r_rd_mem_sel}
  when (w_wr_wrap) { r_wr_mem_sel := !r_wr_mem_sel}

  // Read and/or write to buffers
  if (params.singlePortMem) {
    val w_address_1 = Mux(io.in.fire && (!r_wr_mem_sel), w_wr_addr, r_rd_cnt)
    val w_address_2 = Mux(io.in.fire &&   r_wr_mem_sel, w_wr_addr, r_rd_cnt)

    val w_port_1 = memories.head(w_address_1)
    when(io.in.fire && !r_wr_mem_sel) { w_port_1 := io.in.bits }
    w_mem_data_1 := w_port_1

    val w_port_2 = memories.last(w_address_2)
    when(io.in.fire && r_wr_mem_sel) { w_port_2 := io.in.bits }
    w_mem_data_2 := w_port_2
  } else {
    when(io.in.fire && !r_wr_mem_sel) {
      memories.head(w_wr_addr) := io.in.bits
    }
    when(io.in.fire && r_wr_mem_sel) {
      memories.last(w_wr_addr) := io.in.bits
    }
    w_mem_data_1 := memories.head(r_rd_cnt)
    w_mem_data_2 := memories.last(r_rd_cnt)
  }

  // output logic
  private val buffer = Module(new Queue(chiselTypeOf(w_out), entries = 1, pipe = false, flow = true))
  buffer.io.enq.bits  := w_out
  buffer.io.enq.valid := RegNext(w_valid && io.out.ready ,false.B)
  io.out.valid        := buffer.io.deq.valid
  buffer.io.deq.ready := io.out.ready
  io.out.bits := buffer.io.deq.bits(buffer.io.deq.bits.getWidth - 2, 0).asTypeOf(io.out.bits)
  io.o_last   := buffer.io.deq.bits(buffer.io.deq.bits.getWidth - 1)
  io.in.ready := r_state =/= FSM.s_read
}

object BitReverseApp extends App {

  val params = BitReverseParams(
    dataType = DspComplex(FixedPoint(16.W, 14.BP)),
    memDepth = 8,
    runTime = false
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new BitReverse(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/BitReverse"))
  )
}
