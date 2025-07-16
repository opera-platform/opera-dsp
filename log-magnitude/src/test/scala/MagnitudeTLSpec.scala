package opera.logmagnitude

import breeze.linalg.max
import chisel3.fromIntToWidth
import chisel3.util.log2Ceil
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.StandaloneTLBlock
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeTLSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MagnitudeTL"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes  = 4
  val address    = AddressSet(0x1000, 0xFF)
  val sampleSize = 128
  val verbose    = false
  val random     = true

  for (magType <- Seq(LogSquaredJPL, LogJPLSquared)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (mulPipeRegs <- Seq(false, true)) {
        for (inBinaryPoint <- Seq(8, 10, 12)) {
          for (outBinaryPoint <- Seq(8, 10, 12)) {
            val realBinaryPointSeq = if (magType == JPL || magType == Squared) Seq(None) else Seq(Some(8), Some(10), Some(12))
            val lutTableWidthSeq   = if (magType == JPL || magType == Squared) Seq(None) else Seq(Some(8), Some(12))
            val lutTableSizeSeq    = if (magType == JPL || magType == Squared) Seq(None) else Seq(Some(6), Some(8))
            for (realBinaryPoint <- realBinaryPointSeq) {
              for (lutTableWidth <- lutTableWidthSeq) {
                for (lutTableSize <- lutTableSizeSeq) {
                  val inputWholePart  = 2
                  val realWholePart   = max(2 * inputWholePart + 1, log2Ceil(inBinaryPoint) + 1)
                  val outputWholePart = if (realBinaryPoint.isDefined) 2 * realWholePart + 1 else 2 * inputWholePart + 1
                  // Parameters
                  val params = LogMagnitudeParams[FixedPoint](
                    inputType     = DspComplex(FixedPoint((inputWholePart + inBinaryPoint).W, inBinaryPoint.BP)),
                    realType      = if (realBinaryPoint.isDefined) Some(FixedPoint((realWholePart + realBinaryPoint.get).W, realBinaryPoint.get.BP)) else None,
                    outputType    = FixedPoint((outputWholePart + outBinaryPoint).W, outBinaryPoint.BP),
                    lutTableSize  = lutTableSize,
                    lutTableWidth = lutTableWidth,
                    magType       = magType,
                    addPipeRegs   = addPipeRegs,
                    mulPipeRegs   = false,
                    trimType      = Convergent
                  )
                  // Input data width
                  val inputWidth: Int =
                    if (params.magType == Log) params.realType.get.getWidth
                    else params.inputType.getWidth
                  val inputBytes = math.ceil(inputWidth.toDouble / 8).toInt

                  it should "pass when: \n" +
                    s"\t\tmagType         = $magType, \n" +
                    s"\t\taddPipeRegs     = $addPipeRegs, \n" +
                    s"\t\tmulPipeRegs     = $mulPipeRegs, \n" +
                    s"\t\tinBinaryPoint   = $inBinaryPoint, \n" +
                    s"\t\toutBinaryPoint  = $outBinaryPoint, \n" +
                    s"\t\trealBinaryPoint = $realBinaryPoint, \n" +
                    s"\t\tlutTableSize    = $lutTableSize, \n" +
                    s"\t\tlutTableWidth   = $lutTableWidth, \n" in {

                    val lazyDut = LazyModule(
                      new MagnitudeTL[FixedPoint](
                        address   = address,
                        params    = params,
                        beatBytes = beatBytes
                      ) with StandaloneTLBlock {
                        override def standaloneParams: TLBundleParameters =
                          TLBundleParameters(
                            addressBits    = beatBytes * 8,
                            dataBits       = beatBytes * 8,
                            sourceBits     = 1,
                            sinkBits       = 1,
                            sizeBits       = 2,
                            echoFields     = Seq(),
                            requestFields  = Seq(),
                            responseFields = Seq(),
                            hasBCE         = false
                          )
                        override def dataBytes: Int = inputBytes
                      }
                    )

                    test(lazyDut.module)
                      .withAnnotations(annotations)
                      .runPeekPoke(_ =>
                        new MagnitudeTLTester(
                          dut        = lazyDut,
                          params     = params,
                          sampleSize = sampleSize,
                          verbose    = verbose,
                          random     = random,
                          address    = address,
                          beatBytes  = beatBytes
                        )
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
