package opera.logmagnitude

import chisel3._
import chisel3.experimental.requireIsHardware
import chisel3.util.{DecoupledIO, Queue, ShiftRegister, log2Ceil}

object AlignHandshake {
  def apply[T <: Data](latency: Int, in: DecoupledIO[_ <: Data], out: DecoupledIO[T], en: Bool = true.B): T = {
    // Input and Output must be hardware types
    requireIsHardware(in)
    requireIsHardware(out)
    // Latency cannot be negative
    require(latency >= 0)

    // If there is no latency, just connect input/output ready and valid signals
    if (latency == 0) {
      in.ready  := out.ready
      out.valid := in.valid

      return out.bits
    }
    
    val updatedLatency = if (latency % 2 == 0) latency + 1 else latency

    val queue = Module(new Queue(chiselTypeOf(out.bits), updatedLatency + 1, pipe = updatedLatency == 1))
    val r_counter = RegInit(0.U(log2Ceil(latency + 1).W))
    r_counter := r_counter +& in.fire -& out.fire
    
    queue.io.enq.valid := ShiftRegister(in.fire, latency, false.B, en)
    assert(!queue.io.enq.valid || queue.io.enq.ready) // we control in.ready such that the queue can't fill up!

    in.ready := (r_counter < updatedLatency.U) //|| (r_counter === updatedLatency.U && out.ready)
    queue.io.deq.ready := out.ready
    out.valid := queue.io.deq.valid
    out.bits := queue.io.deq.bits

    // Return
    queue.io.enq.bits
  }
}
