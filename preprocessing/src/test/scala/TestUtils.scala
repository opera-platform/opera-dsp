package preprocessing

import scala.annotation.tailrec

trait TestUtils {
  // Reverse model. Reverse bits in data
  def reverse(x: Seq[BigInt], en: Boolean, dataBytes: Int): Seq[BigInt] = {
    if (en) {
      val shiftBits = dataBytes * 8
      val mask = (BigInt(1) << shiftBits) - 1
      x.map(m => {
        val x_masked = m & mask
        var reversed = BigInt(0)
        for (i <- 0 until shiftBits) {
          // If bit i in x_masked is set, set bit (shiftBits - 1 - i) in reversed
          if ((x_masked & (BigInt(1) << i)) != 0) {
            reversed = reversed.setBit(shiftBits - 1 - i)
          }
        }
        reversed
      })
    } else {
      x
    }
  }

  // Swap model. Swap upper and lower parts of the data
  def swap(data: Seq[BigInt], format: Int, en: Boolean, dataBytes: Int): Seq[BigInt] = {
    val shiftBits = dataBytes * 8
    val mask = (1 << shiftBits) - 1
    val swappedData = format match {
      case 0 => data.map(m => m & mask) // Real data on the lower half
      case 1 =>
        if (en) {
          // Swap values
          data.grouped(2).toSeq.map(m => ((m.head & mask) << shiftBits) | (m.last & mask))
        } else {
          // Don't swap values
          data.grouped(2).toSeq.map(m => ((m.last & mask) << shiftBits) | (m.head & mask))
        }
      case _ => data.grouped(2).toSeq.map(m => ((m.last & mask) << shiftBits) | (m.head & mask))
    }
    swappedData
  }

  // Padder model
  def pad(data: Seq[BigInt], en: Boolean, samplesPerChirp: Int, expectedSamples: Int, chirpsPerFrame: Int): Seq[BigInt] = {
    if (en) {
      data.grouped(expectedSamples).flatMap { chunk =>
        chunk
          .padTo(expectedSamples, BigInt(0))
          .padTo(samplesPerChirp, BigInt(0))
      }.toSeq
    }
    else data
  }

  private def reflect(data: Long, bits: Int): Long = {
    @tailrec
    def loop(i: Int, result: Long): Long = {
      if (i == 0) result
      else {
        val bit = (data >> (bits - i)) & 1
        loop(i - 1, (result << 1) | bit)
      }
    }

    loop(bits, 0)
  }

  def crc32(data: Array[Byte], params: CRCParameters, crcWidth: Int): Long = {
    val topBit = 1L << (crcWidth - 1)
    val mask = if (crcWidth >= 64) -1L else (1L << crcWidth) - 1
    var crc = params.init & mask

    for (b <- data) {
      val byte = if (params.reflectIn) reflect(b & 0xFF, 8) else b & 0xFF
      crc ^= (byte << (crcWidth - 8)) & mask
      for (_ <- 0 until 8) {
        crc = if ((crc & topBit) != 0) ((crc << 1) ^ params.polynomial) & mask
        else (crc << 1) & mask
      }
    }

    if (params.reflectOut) crc = reflect(crc, crcWidth)
    (crc ^ params.xorOut) & mask
  }

  // PreProcessing model
  def transform(
                 data: Seq[BigInt],
                 ctrl: Int, format: Int,
                 dataBytes: Int,
                 samplesPerChirp: Int,
                 expectedSamples: Int,
                 chirpsPerFrame: Int
               ): Seq[BigInt] = {
    // Reverse data
    val data_reverse = reverse(data, (ctrl & 0x2) == 2, dataBytes/2)
    // Swap the data
    val data_swap = swap(data_reverse, format, (ctrl & 0x4) == 4 , dataBytes/2)
    // Pad the data
    val data_padded = pad(data_swap, (ctrl & 0x8) == 8, samplesPerChirp, expectedSamples, chirpsPerFrame)
    //Return
    data_padded
  }

  // Format string
  def formatString(data: BigInt, dataBytes:Int): String = {
    // Determine how many hex numbers wy need to print dataBytes number of Bytes
    val hexNumbers = dataBytes * 2
    // Convert BigInt to uppercase Hex
    val peekedString = data.toString(16).toUpperCase
    // Fill with zeroes
    if (peekedString.length >= hexNumbers) peekedString
    else "0" * (hexNumbers - peekedString.length) + peekedString
  }

  // Create sub sequence of length M from given sequence X
  def createSubSequence(X: Seq[Int], M: Int): Seq[Int] = {
    val N = X.length
    require(N > 0, "Sequence X must not be empty.")
    require(M > 0, "M must be greater than 0.")

    if (M >= N) {
      X // If requested M exceeds or equals available elements, return all elements.
    } else {
      val indexes = (0 until M).map(i => ((i.toDouble * (N - 1)) / (M - 1)).round.toInt)
      indexes.map(X)
    }
  }
}

case class RegConfiguration (
  chirpsize: Int,
  expectedsize: Int,
  chirpperframe: Int,
  dataformat: Int,
  ctrl: Int
) {
  assert(dataformat >= 0 && dataformat <=3, f"Data format cannot be $dataformat, it must be between 0 and 3")
  assert(ctrl >= 0 && ctrl <=0xF, f"Control cannot be $ctrl, it must be between 0 and 0xF")
}

case class TestConfiguration (
  regs: Seq[RegConfiguration]
)
