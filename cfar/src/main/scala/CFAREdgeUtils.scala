package opera.cfar

import chisel3._

private[cfar] object CFAREdgeUtils {
  def edgeSpan(referenceCells: UInt, guardCells: UInt): UInt = referenceCells +& guardCells

  def isLeftEdge(index: UInt, span: UInt): Bool = index < span

  def isRightEdge(index: UInt, fftSize: UInt, span: UInt): Bool = index >= fftSize - span

  def assertActiveWindowFits(fftSize: UInt, referenceCells: UInt, guardCells: UInt): Unit = {
    assert(
      fftSize > 2.U * referenceCells + 2.U * guardCells + 1.U,
      "i_fft_size must be larger than 2*reference_cells + 2*guard_cells + 1"
    )
  }
}
