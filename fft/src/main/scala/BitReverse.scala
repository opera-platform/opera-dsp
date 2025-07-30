package opera.fft

import breeze.numerics.pow
import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import _root_.circt.stage.{ChiselStage, FirtoolOption}
import dspblocks._
import dsptools.numbers.{BinaryRepresentation, DspComplex, Real, binaryRepresentationOps}
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

// TODO: FIX ready/valid!
case class BitReverseParams[T <: Data](
  dataType: DspComplex[T], // data type
  memDepth:   Int, // ping pong memDepth or fft memDepth
  runTime: Boolean = false,
  singlePortMem: Boolean = false
) {
  requireIsChiselType(dataType)
}

object BitReverseParams {
  def fixedPoint(
    dataType: DspComplex[FixedPoint] = DspComplex(FixedPoint(16.W, 14.BP)),
    memDepth:   Int = 16,
    runTime: Boolean = false,
    singlePortMem: Boolean = false
  ): BitReverseParams[FixedPoint] = {
    BitReverseParams(
      dataType = dataType,
      memDepth = memDepth,
      runTime = runTime,
      singlePortMem = singlePortMem
    )
  }
}

class BitReverseIO[T <: Data: Real](val params: BitReverseParams[T]) extends Bundle {
  val in: DecoupledIO[DspComplex[T]] = Flipped(Decoupled(params.dataType))
  val out: DecoupledIO[DspComplex[T]] = Decoupled(params.dataType)
  val i_samples: Option[UInt] = if (params.runTime) Some(Input(UInt(log2Ceil(params.memDepth + 1).W))) else None

  val i_last: Bool = Input(Bool())
  val o_last: Bool = Output(Bool())
}

class BitReverse[T <: Data: Real: BinaryRepresentation](val params: BitReverseParams[T]) extends Module {
  val io: BitReverseIO[T] = IO(new BitReverseIO(params))

  private val w_valid = Wire(Bool())
  // Double buffer
  val memDepth: Int        = params.memDepth
  private val memories     = Seq.fill(2) { SyncReadMem(memDepth, params.dataType) } // Double buffer
  private val r_mem_empty  = Seq.fill(2) { RegInit(true.B)  } // Empty flag for memories
  private val r_mem_full   = Seq.fill(2) { RegInit(false.B) } // Full flag for memories
  private val w_mem_data_1 = Wire(params.dataType)
  private val w_mem_data_2 = Wire(params.dataType)
  
  // FSM
  private object FSM extends ChiselEnum {
    val s_idle, s_write, s_write_read, s_read = Value
  }
  private val r_state = RegInit(FSM.s_idle)

  // Logic and Counters for reading and writing to memories
  private val w_wr_cnt_rst = Wire(Bool()) // write counter reset
  private val w_rd_cnt_rst = Wire(Bool()) // read counter reset
  private val w_wr_wrap    = Wire(Bool()) // wrap the value of write counter
  private val w_rd_wrap    = Wire(Bool()) // wrap the value of read counter
  private val w_rd_en      = Wire(Bool())
  private val r_wr_cnt     = CounterWithReset(io.in.fire , memDepth, w_wr_cnt_rst)._1 // write counter with reset
  private val r_rd_cnt     = CounterWithReset(w_rd_en, memDepth, w_rd_cnt_rst)._1 // read counter with reset
  private val w_rd_addr: UInt = Wire(r_rd_cnt.cloneType)
  private val w_wr_addr: UInt = Wire(r_wr_cnt.cloneType)
  private val r_wr_mem_sel: Bool = RegInit(false.B) // select in which memory to write
  private val w_rd_mem_sel: Bool = Wire(Bool())     // select from which memory to read
  dontTouch(r_wr_cnt)
  dontTouch(r_rd_cnt)
  dontTouch(w_wr_cnt_rst)
  dontTouch(w_rd_cnt_rst)
  // Conditions to wrap and reset counters
  w_wr_wrap    := r_wr_cnt === (io.i_samples.getOrElse(memDepth.U) - 1.U) && io.in.fire
  w_rd_wrap    := r_rd_cnt === (io.i_samples.getOrElse(memDepth.U) - 1.U) && io.out.fire
  w_wr_cnt_rst := w_wr_wrap || r_state === FSM.s_idle
  w_rd_cnt_rst := w_rd_wrap || r_state === FSM.s_idle

  // Generate write and read address
  private val fftStageNumber = log2Ceil(memDepth)
  private val fftSizes = (1 to fftStageNumber).map(n => pow(2, n).U)
  private val w_mux_case_map = fftSizes.map(
    m => m === io.i_samples.getOrElse(memDepth.U)
  ).zip(1 to fftStageNumber).map {
    case (condition, numBits) =>
      condition -> Reverse(r_wr_cnt(numBits - 1, 0))
  }
  w_wr_addr := MuxCase(0.U(fftStageNumber.W), w_mux_case_map)
  w_rd_addr := r_rd_cnt

  io.in.ready := true.B
  w_valid:= false.B
  w_rd_mem_sel := !r_wr_mem_sel

  when(r_state === FSM.s_idle) {
    // Reset state
    io.in.ready  := true.B
    w_valid := false.B
    r_state      := FSM.s_write
  }.elsewhen(r_state === FSM.s_write) {
    // Memories are empty, we can only write to them
    io.in.ready  := true.B
    w_valid := false.B
    when (w_wr_wrap) {
      r_state      := FSM.s_write_read
      r_wr_mem_sel := !r_wr_mem_sel
    }
  }.elsewhen(r_state === FSM.s_read) {
    // Memories are full, we can only read from them
    io.in.ready := false.B
    w_valid := true.B
    when(w_rd_wrap) {
      r_state      := FSM.s_write_read
      r_wr_mem_sel := !r_wr_mem_sel
    }
  }.elsewhen(r_state === FSM.s_write_read) {
    // We can read from one memory and write to another
    io.in.ready  := true.B
    w_valid := true.B
    // Change state
    when(r_mem_empty.head && r_mem_empty.last) {
      r_state      := FSM.s_write
      r_wr_mem_sel := !r_wr_mem_sel
    }.elsewhen(r_mem_full.head && r_mem_full.last) {
      r_state      := FSM.s_read
      r_wr_mem_sel := !r_wr_mem_sel
    }.elsewhen(w_wr_wrap && w_rd_wrap) {
      r_wr_mem_sel := !r_wr_mem_sel
    }
  }

  // Read/Write to double buffer memories
  when(r_wr_mem_sel === 0.U) {
    when(w_wr_wrap)   { r_mem_full.head  := true.B }
    when(io.out.fire) { r_mem_full.last  := false.B }
    when(w_rd_wrap)   { r_mem_empty.last := true.B }
    when(io.in.fire)  { r_mem_empty.head := false.B }
  }.otherwise {
    when(w_wr_wrap)   { r_mem_full.last  := true.B }
    when(io.out.fire) { r_mem_full.head  := false.B }
    when(w_rd_wrap)   { r_mem_empty.head := true.B }
    when(io.in.fire)  { r_mem_empty.last := false.B }
  }

  if (params.singlePortMem) {
    val w_address_1 = Mux(r_wr_mem_sel, w_wr_addr, w_rd_addr)
    val w_address_2 = Mux(!r_wr_mem_sel, w_wr_addr, w_rd_addr)
    when(io.in.fire && r_wr_mem_sel) {
      memories.last(w_address_1) := io.in.bits
    }
    when(io.in.fire && !r_wr_mem_sel) {
      memories.head(w_address_2) := io.in.bits
    }
    w_mem_data_1 := memories.last(w_address_1)
    w_mem_data_2 := memories.head(w_address_2)
  } else {
    when(io.in.fire && !r_wr_mem_sel) {
      memories.head(w_wr_addr) := io.in.bits
    }
    when(io.in.fire && r_wr_mem_sel) {
      memories.last(w_wr_addr) := io.in.bits
    }
    w_mem_data_1 := memories.head(w_rd_addr)
    w_mem_data_2 := memories.last(w_rd_addr)
  }
  dontTouch(w_mem_data_1)
  dontTouch(w_mem_data_2)


  w_rd_en := w_valid && io.out.ready
  io.o_last := w_rd_wrap
  io.out.bits := Mux(w_rd_mem_sel, w_mem_data_2, w_mem_data_1)
  io.out.valid := RegNext(w_valid)
}

object BitReverseApp extends App {

  val params = BitReverseParams.fixedPoint(
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
