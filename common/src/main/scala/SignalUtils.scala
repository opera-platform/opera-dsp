package opera.common

import breeze.math.Complex

import scala.math.BigDecimal.long2bigDecimal
import scala.util.Random

trait SignalUtils {

  // Generates complex or real sinusoid with optional added noise
  def generateSignal(
    numSamples   : Int,
    freqReal1    : Double,
    freqReal2    : Double = 0,
    freqImag1    : Double = 0,
    freqImag2    : Double = 0,
    addNoise     : Double = 0,
    scalingFactor: Int = 1
  ): Seq[Complex] = {
    require(freqReal1 != 0, "Frequency should not be zero!")

    (0 until numSamples).map(i =>
      Complex(
        // Real part
        (math.sin(2 * math.Pi * freqReal1 * i) + math.sin(2 * math.Pi * freqReal2 * i)) / scalingFactor + addNoise * ((Random.nextDouble() * 2.0) - 1.0),
        // Imaginary part
        (math.sin(2 * math.Pi * freqImag1 * i) + math.sin(2 * math.Pi * freqImag2 * i)) / scalingFactor + addNoise * ((Random.nextDouble() * 2.0) - 1.0)
      )
    )
  }
  
  // Int to binary
  def intToBinary(source: Int, digits: Int): String = {
    val lstring = source.toBinaryString
    if (source >= 0) {
      val l: java.lang.Long = lstring.toLong
      String.format("%0" + digits + "d", l)
    } else
      lstring.takeRight(digits)
  }

  // Complex data sequence to AXI4-Stream data sequence
  def complexToAXI4StreamSequence(inData: Seq[Complex], dataWidth: Int): Seq[BigInt] = {
    inData.map(data =>
      java.lang.Long.parseLong(
        intToBinary(data.real.toInt, dataWidth) ++ intToBinary(data.imag.toInt, dataWidth), 2
      ).toBigInt
    )
  }
}
