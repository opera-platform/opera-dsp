package opera.logmagnitude

import chisel3._
import chisel3.experimental.requireIsHardware

object AlignHandshake {
  def apply[T <: Data](latency: Int, i_valid: Bool, o_ready: Bool): (Vec[Bool], Vec[Bool]) = {
    // i_valid and o_ready must be hardware
    requireIsHardware(i_valid)
    requireIsHardware(o_ready)
    // Latency must be larger than 0
    require(latency > 0)

    val w_en = Wire(Vec(latency, Bool()))
    val r_valid = RegInit(VecInit(Seq.fill(latency)(false.B)))

    for (i <- 0 until latency) {
      when(w_en(i)) {
        if (i == 0)
          r_valid(i) := i_valid
        else
          r_valid(i) := r_valid(i - 1)
      }

      if (i == latency - 1)
        w_en(i) := o_ready || !r_valid(i)
      else
        w_en(i) := w_en(i + 1) || !r_valid(i)
    }

    (w_en, r_valid)
  }
}
