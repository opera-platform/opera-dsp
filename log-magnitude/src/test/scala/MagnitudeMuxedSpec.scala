package opera.logmagnitude

import breeze.linalg.max
import chisel3.fromIntToWidth
import chisel3.util.log2Ceil
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeMuxedSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MagnitudeMuxed"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val sampleSize  = 256
  val verbose     = false
  val random      = true
  val dataRandom  = true

  for (magType <- Seq(LogSquaredJPL, LogJPLSquared)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (mulPipeRegs <- Seq(false, true)) {
        for (inBinaryPoint <- Seq(6, 10, 14)) {
          for (outBinaryPoint <- Seq(6, 10, 14)) {
            val lutTableWidthSeq = if (magType == JPL || magType == Squared) Seq(None) else Seq(Some(8), Some(10), Some(12))
            val lutTableSizeSeq  = if (magType == JPL || magType == Squared) Seq(None) else Seq(Some(4), Some(6), Some(8))
            for (lutTableWidth <- lutTableWidthSeq) {
              for (lutTableSize <- lutTableSizeSeq) {
                for (select <- Seq(0, 1)) {
                  val inputWholePart = 2
                  val realWholePart = max(2 * inputWholePart + 1, log2Ceil(inBinaryPoint) + 1)
                  val outputWholePart = 2 * realWholePart + 1
                  // Parameters
                  val params = LogMagnitudeParams[FixedPoint](
                    inputType     = DspComplex(FixedPoint((inputWholePart + inBinaryPoint).W, inBinaryPoint.BP)),
                    realType      = Some(FixedPoint((realWholePart + inBinaryPoint).W, inBinaryPoint.BP)),
                    outputType    = FixedPoint((outputWholePart + outBinaryPoint).W, outBinaryPoint.BP),
                    lutTableSize  = lutTableSize,
                    lutTableWidth = lutTableWidth,
                    magType       = magType,
                    addPipeRegs   = addPipeRegs,
                    mulPipeRegs   = false,
                    trimType      = Convergent
                  )

                  it should "pass when: \n" +
                    s"\t\tmagType         = $magType, \n" +
                    s"\t\taddPipeRegs     = $addPipeRegs, \n" +
                    s"\t\tmulPipeRegs     = $mulPipeRegs, \n" +
                    s"\t\tlutTableSize    = $lutTableSize, \n" +
                    s"\t\tlutTableWidth   = $lutTableWidth, \n" +
                    s"\t\tinBinaryPoint   = $inBinaryPoint, \n" +
                    s"\t\toutBinaryPoint  = $outBinaryPoint, \n" +
                    s"\t\tselect          = $select \n" in {

                    test(new MagnitudeMuxed[FixedPoint](params = params))
                      .withAnnotations(annotations)
                      .runPeekPoke(c =>
                        new MagnitudeMuxedTester(
                          dut        = c,
                          params     = params,
                          sampleSize = sampleSize,
                          select     = select,
                          verbose    = verbose,
                          random     = random,
                          dataRandom = dataRandom
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
