package preprocessing

import chisel3.{UInt, fromIntToLiteral}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBuffer, AXI4StreamBundle, AXI4StreamBundleParameters, AXI4StreamMasterParameters, AXI4StreamSlaveParameters, AXI4StreamToBundleBridge, BundleBridgeToAXI4Stream}
import org.chipsalliance.diplomacy.bundlebridge.{BundleBridgeSink, BundleBridgeSource}
import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule}

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
    // Determine how meny hex numbers wy need to print dataBytes number of Bytes
    val hexNumbers = dataBytes * 2
    // Convert BigInt to uppercase Hex
    val peekedString = data.toString(16).toUpperCase
    // Fill with zeroes
    if (peekedString.length >= hexNumbers) peekedString
    else "0" * (hexNumbers - peekedString.length) + peekedString
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

// AXI4StreamBlock Standalone wrapper for test
trait TestAXI4StreamBlock extends AXI4StreamBlock {
  def dataBytes = 4

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = dataBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode := AXI4StreamBuffer(1) :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = dataBytes)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}
