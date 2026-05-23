package opera.cfar

import chisel3._

private[cfar] object CFARUtils {
  def elasticPipeline[A <: Data](
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
}
