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

// TODO: check io.in.valid and io.out.ready conditions
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

  // Double buffer
  val memDepth: Int = params.memDepth
  private val memories = Seq.fill(2) { SyncReadMem(memDepth, params.dataType) } // Double buffer
  private val w_mem_data_1 = Wire(params.dataType)
  private val w_mem_data_2 = Wire(params.dataType)
  
  // FSM
  private object StateFSM extends ChiselEnum {
    val sIdle, sWriteOnly, sReadWrite, sReadOnly = Value
  }
  private val r_state = RegInit(StateFSM.sIdle)
  private val w_next_state = WireInit(StateFSM.sIdle)

  // Logic and Counters for reading and writing to memories
  private val w_wr_cnt_en  = Wire(Bool()) // write counter enable
  private val w_rd_cnt_en  = Wire(Bool()) // read counter enable
  private val w_wr_cnt_rst = Wire(Bool()) // write counter reset
  private val w_rd_cnt_rst = Wire(Bool()) // read counter reset
  private val w_wr_wrap    = Wire(Bool()) // wrap the value of write counter
  private val w_rd_wrap    = Wire(Bool()) // wrap the value of read counter
  private val r_wr_cnt     = CounterWithReset(io.in.valid  && w_wr_cnt_en, memDepth, w_wr_cnt_rst)._1 // write counter with reset
  private val r_rd_cnt     = CounterWithReset(io.out.ready && w_rd_cnt_en, memDepth, w_rd_cnt_rst)._1 // read counter with reset
  private val w_rd_addr: UInt = Wire(r_rd_cnt.cloneType)
  private val w_wr_addr: UInt = Wire(r_wr_cnt.cloneType)
  private val r_rd_mem_sel = RegInit(false.B) // select from which memory to read
  private val r_wr_mem_sel = RegInit(false.B) // select in which memory to write
  // Conditions to wrap and reset counters
  w_wr_cnt_en  := r_state =/= StateFSM.sReadOnly
  w_rd_cnt_en  := r_state === StateFSM.sReadOnly || r_state === StateFSM.sReadWrite
  w_wr_wrap    := r_wr_cnt === (io.i_samples.getOrElse(memDepth.U) - 1.U)
  w_rd_wrap    := r_rd_cnt === (io.i_samples.getOrElse(memDepth.U) - 1.U)
  w_wr_cnt_rst := w_wr_wrap && (io.in.fire && w_wr_cnt_en) || w_next_state === StateFSM.sIdle
  w_rd_cnt_rst := w_rd_wrap && (io.out.fire && w_rd_cnt_en) || w_next_state === StateFSM.sIdle

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

  // Read/Write to double buffer memories
  if (params.singlePortMem) {
    val w_address_1 = Mux( r_wr_mem_sel, w_wr_addr, w_rd_addr)
    val w_address_2 = Mux(!r_wr_mem_sel, w_wr_addr, w_rd_addr)
    when(io.in.fire &&  r_wr_mem_sel) { memories.last(w_address_1) := io.in.bits }
    when(io.in.fire && !r_wr_mem_sel) {  memories.head(w_address_2) := io.in.bits }
    w_mem_data_1 := memories.last(w_address_1)
    w_mem_data_2 := memories.head(w_address_2)
  } else {
    when(io.in.fire && !r_wr_mem_sel) { memories.head(w_wr_addr) := io.in.bits }
    when(io.in.fire &&  r_wr_mem_sel) { memories.last(w_wr_addr) := io.in.bits }
    w_mem_data_2 := memories.head(w_rd_addr)
    w_mem_data_1 := memories.last(w_rd_addr)
  }

  // TODO: FSM conditions

}

object BitReverseApp extends App {

  val params = BitReverseParams.fixedPoint(
    dataType = DspComplex(FixedPoint(16.W, 14.BP)),
    memDepth = 1024,
    runTime = true
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new BitReverse(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/BitReverse"))
  )
}
