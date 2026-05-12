package opera.fft

import breeze.math.Complex

object TestLog {
  def verbose: Boolean = TestConfig.verbose

  def log(message: => String): Unit =
    if (verbose) println(message)

  def printStageCompareHeader(title: String): Unit =
    log(s"\n== $title ==")

  def printStageCompareRow(
      cycle          : Int,
      counter        : Int,
      enable         : Boolean,
      divBy2         : Boolean,
      expectedRawReal: BigInt,
      expectedRawImag: BigInt,
      receivedRawReal: BigInt,
      receivedRawImag: BigInt,
      expected       : Complex,
      received       : Complex,
  ): Unit =
    log(
      f"cycle=$cycle%4d counter=$counter%4d enable=$enable div2=$divBy2 " +
        s"expectedRaw=($expectedRawReal, $expectedRawImag) " +
        s"peekedRaw=($receivedRawReal, $receivedRawImag) " +
        f"expected=(${expected.real}%.8f, ${expected.imag}%.8f) " +
        f"peeked=(${received.real}%.8f, ${received.imag}%.8f)"
    )
}
