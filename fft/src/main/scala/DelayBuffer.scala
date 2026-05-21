package opera.fft

import chisel3._
import chisel3.util.ShiftRegister
import craft.ShiftRegisterMem

object DelayBuffer {
  def apply[T <: Data](in: T, n: Int, en: Bool = true.B, use_sp_mem: Boolean = false, mem: Boolean = false, name: String = null): T =
    if (mem) ShiftRegisterMem(in, n, en, use_sp_mem, name) else ShiftRegister(in, n, en)
}
