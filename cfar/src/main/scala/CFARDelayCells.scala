package opera.cfar

import chisel3._
import chisel3.experimental.{requireIsChiselType, requireIsHardware}
import chisel3.util._

object DelayRegisterCells {
  def apply[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, enable: Bool = true.B): T = {
    withTaps(in, maxDepth, depth, resetData, enable)._1
  }

  def withTaps[T <: Data](in: T, maxDepth: Int, depth: UInt, resetData: T, enable: Bool = true.B): (T, Vec[T]) = {
    require(maxDepth > 0, s"maxDepth must be positive, got $maxDepth")
    requireIsHardware(in)
    assert(depth <= maxDepth.U)

    val taps = RegInit(VecInit(Seq.fill(maxDepth)(resetData)))
    for (index <- 0 until maxDepth) {
      val next = if (index == 0) in else taps(index - 1)
      when(enable && depth > index.U) {
        taps(index) := next
      }
    }

    val delayed = Mux1H((1 to maxDepth).map { depthValue =>
      (depth === depthValue.U) -> taps(depthValue - 1)
    })
    (delayed, taps)
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
    val i_data = Flipped(Decoupled(dataType.cloneType))
    val i_last = Input(Bool())

    val o_data = Decoupled(dataType.cloneType)
    val o_last = Output(Bool())
    val o_full = Output(Bool())
    val o_empty = Output(Bool())
  })

  private val useSRAM = minSRAMDepth < maxDepth
  if (useSRAM) {
    val delay = Module(new DelaySRAMCells(dataType, maxDepth))
    delay.io.i_depth := io.i_depth
    delay.io.i_data <> io.i_data
    delay.io.i_last := io.i_last
    io.o_data <> delay.io.o_data
    io.o_last := delay.io.o_last
    io.o_full := delay.io.o_full
    io.o_empty := delay.io.o_empty
  } else {
    val delay = Module(new DelayRegisterCells(dataType, maxDepth))
    delay.io.i_depth := io.i_depth
    delay.io.i_data <> io.i_data
    delay.io.i_last := io.i_last
    io.o_data <> delay.io.o_data
    io.o_last := delay.io.o_last
    io.o_full := delay.io.o_full
    io.o_empty := delay.io.o_empty
  }
}

class DelayRegisterCells[T <: Data](val dataType: T, val maxDepth: Int) extends Module {
  require(maxDepth > 1, s"maxDepth must be greater than 1, got $maxDepth")
  requireIsChiselType(dataType)

  val io = IO(new Bundle {
    val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))
    val i_data = Flipped(Decoupled(dataType.cloneType))
    val i_last = Input(Bool())

    val o_data = Decoupled(dataType.cloneType)
    val o_last = Output(Bool())
    val o_taps = Output(Vec(maxDepth, dataType.cloneType))
    val o_full = Output(Bool())
    val o_empty = Output(Bool())
  })

  val inputCount = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val initialFillDone = RegInit(false.B)
  val draining = RegInit(false.B)
  val resetData = 0.U.asTypeOf(io.i_data.bits)
  val enable = io.i_data.fire || (draining && io.o_data.ready)

  val (delayedSample, taps) =
    DelayRegisterCells.withTaps(io.i_data.bits, maxDepth, io.i_depth, resetData, enable)

  when(io.i_last && io.i_data.fire) {
    draining := true.B
  }

  when(io.i_data.fire) {
    inputCount := inputCount + 1.U
  }

  when(io.i_depth > 1.U) {
    when(inputCount === io.i_depth - 1.U && io.i_data.fire) {
      initialFillDone := true.B
    }
  }.otherwise {
    when(io.i_data.fire && io.i_depth === 1.U) {
      initialFillDone := true.B
    }
  }

  val fireLastIn = io.i_last && io.i_data.fire
  val lastOut = DelayRegisterCells(fireLastIn, maxDepth, io.i_depth, resetData = false.B, enable = io.o_data.fire)

  when(lastOut && io.o_data.fire) {
    inputCount := 0.U
    initialFillDone := false.B
    draining := false.B
  }

  io.o_empty := inputCount === 0.U && !initialFillDone
  io.o_full := initialFillDone && !draining
  io.i_data.ready := Mux(io.i_depth === 0.U, io.o_data.ready, !initialFillDone || io.o_data.ready && !draining)
  io.o_data.bits := Mux(io.i_depth === 0.U, io.i_data.bits, delayedSample)
  io.o_data.valid := Mux(io.i_depth === 0.U, io.i_data.valid, initialFillDone && io.i_data.valid || (draining && enable))
  io.o_last := Mux(io.i_depth === 0.U, io.i_last && io.i_data.fire, lastOut)
  io.o_taps := taps
}

class DelaySRAMCells[T <: Data](val dataType: T, val maxDepth: Int) extends Module {
  require(maxDepth > 1, s"maxDepth must be greater than 1, got $maxDepth")
  requireIsChiselType(dataType)

  val io = IO(new Bundle {
    val i_depth = Input(UInt(log2Ceil(maxDepth + 1).W))
    val i_data = Flipped(Decoupled(dataType.cloneType))
    val i_last = Input(Bool())

    val o_data = Decoupled(dataType.cloneType)
    val o_last = Output(Bool())
    val o_full = Output(Bool())
    val o_empty = Output(Bool())
  })

  val mem = SyncReadMem(maxDepth, dataType)
  val outputQueue = Module(new Queue(dataType.cloneType, 2, pipe = true, flow = true))
  val writeIndex = RegInit(0.U(log2Ceil(maxDepth).W))
  val inputCount = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val initialFillDone = RegInit(false.B)
  val draining = RegInit(false.B)
  val resetData = 0.U.asTypeOf(io.i_data.bits)

  val outputFire = io.o_data.fire
  val activeOutputReady = io.o_data.ready && outputQueue.io.enq.ready

  io.i_data.ready := Mux(io.i_depth === 0.U, io.o_data.ready, !initialFillDone || (activeOutputReady && !draining))

  val fireLastIn = io.i_last && io.i_data.fire
  val lastOut = DelayRegisterCells(fireLastIn, maxDepth, io.i_depth, resetData = false.B, enable = outputFire)
  val resetAll = lastOut && outputFire
  val drainAdvance = draining && activeOutputReady && outputQueue.io.deq.valid && !resetAll
  val advance = io.i_data.fire || drainAdvance

  val readAddress = Mux(
    writeIndex + 1.U >= io.i_depth,
    writeIndex + 1.U - io.i_depth,
    maxDepth.U + writeIndex + 1.U - io.i_depth
  )(log2Ceil(maxDepth) - 1, 0)
  val readFromMemory = advance && io.i_depth > 1.U
  val memoryData = mem.read(readAddress, readFromMemory)
  val directData = Mux(io.i_data.fire, io.i_data.bits, resetData)
  val responseUsesDirectData = RegEnable(io.i_depth <= 1.U, false.B, advance)
  val directDataResponse = RegEnable(directData, resetData, advance)
  val responseValid = RegNext(
    Mux(
      io.i_depth === 0.U,
      io.i_data.fire,
      (inputCount === io.i_depth - 1.U && io.i_data.fire) ||
        (initialFillDone && io.i_data.fire) ||
        drainAdvance
    ),
    init = false.B
  )

  when(advance && !resetAll) {
    mem.write(writeIndex, directData)
    writeIndex := Mux(writeIndex === (maxDepth - 1).U, 0.U, writeIndex + 1.U)
  }

  when(io.i_data.fire) {
    inputCount := inputCount + 1.U
  }

  when(io.i_depth > 1.U) {
    when(inputCount === io.i_depth - 1.U && io.i_data.fire) {
      initialFillDone := true.B
    }
  }.otherwise {
    when(io.i_data.fire && io.i_depth <= 1.U) {
      initialFillDone := true.B
    }
  }

  when(fireLastIn) {
    draining := true.B
  }

  when(resetAll) {
    writeIndex := 0.U
    inputCount := 0.U
    initialFillDone := false.B
    draining := false.B
  }

  outputQueue.io.enq.valid := responseValid
  outputQueue.io.enq.bits := Mux(responseUsesDirectData, directDataResponse, memoryData)
  outputQueue.io.deq.ready := io.o_data.ready

  io.o_data.bits := Mux(io.i_depth === 0.U, io.i_data.bits, outputQueue.io.deq.bits)
  io.o_data.valid := Mux(io.i_depth === 0.U, io.i_data.valid, outputQueue.io.deq.valid)
  io.o_last := Mux(io.i_depth === 0.U, io.i_last && io.i_data.fire, lastOut)
  io.o_full := initialFillDone && !draining
  io.o_empty := inputCount === 0.U && !initialFillDone
}

class CFARCutDelay[T <: Data](val dataType: T) extends Module {
  requireIsChiselType(dataType)

  val io = IO(new Bundle {
    val i_data = Flipped(Decoupled(dataType.cloneType))
    val i_last = Input(Bool())
    val o_data = Decoupled(dataType.cloneType)
    val o_last = Output(Bool())
  })

  val initialFillDone = RegInit(false.B)
  val draining = RegInit(false.B)
  val resetData = 0.U.asTypeOf(io.i_data.bits)
  val enable = io.i_data.fire || (draining && io.o_data.ready)
  val cut = RegEnable(io.i_data.bits, resetData, enable)

  when(io.i_data.fire && !initialFillDone) {
    initialFillDone := true.B
  }

  when(io.i_last && io.i_data.fire) {
    draining := true.B
  }

  val fireLastIn = io.i_last && io.i_data.fire
  val lastOut = RegEnable(fireLastIn, false.B, io.o_data.ready)

  when(lastOut && io.o_data.ready) {
    initialFillDone := false.B
    draining := false.B
  }

  io.i_data.ready := !initialFillDone || io.o_data.ready && !draining
  io.o_data.bits := cut
  io.o_data.valid := initialFillDone && io.i_data.valid || (draining && enable)
  io.o_last := lastOut
}
