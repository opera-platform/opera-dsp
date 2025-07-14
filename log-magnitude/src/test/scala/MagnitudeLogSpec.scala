package opera.logmagnitude

import chisel3.fromIntToWidth
import chisel3.util.log2Ceil
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeLogSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MagnitudeLog"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val sampleSize  = 128
  val verbose     = true
  val random      = true
  val dataRandom  = true

  for (magType <- Seq(Log)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (lutTableSize <- Seq(4, 8)) {
        for (outBinaryPoint <- Seq(8, 10, 12)) {
          for (inputBinaryPoint <- Seq(8, 10, 12)) {
            for (lutTableWidth <- Seq(8, 10, 12)) {
              for (inputWholePart <- Seq(2, 3, 4)) {
                for (extendOut <- Seq(0, 1, 2)) {
                  val outputWholePart = log2Ceil(inputBinaryPoint) + 1 + extendOut
                  // Parameters
                  val params = LogMagnitudeParams[FixedPoint](
                    inputType     = DspComplex(FixedPoint(16.W, 0.BP)), // This doesn't matter for Log
                    realType      = Some(FixedPoint((inputWholePart + inputBinaryPoint).W, inputBinaryPoint.BP)),
                    outputType    = FixedPoint((outputWholePart + outBinaryPoint).W, outBinaryPoint.BP),
                    lutTableSize  = lutTableSize,
                    lutTableWidth = Some(lutTableWidth),
                    magType       = magType,
                    addPipeRegs   = addPipeRegs,
                    trimType      = Convergent
                  )

                  it should "pass when: \n" +
                    s"\t\tmagType          = $magType, \n" +
                    s"\t\taddPipeRegs      = $addPipeRegs, \n" +
                    s"\t\tlutTableSize     = $lutTableSize, \n" +
                    s"\t\toutputWholePart  = $outputWholePart, \n" +
                    s"\t\toutBinaryPoint   = $outBinaryPoint, \n" +
                    s"\t\tinputWholePart   = $inputWholePart, \n" +
                    s"\t\tinputBinaryPoint = $inputBinaryPoint, \n" +
                    s"\t\tlutTableWidth    = $lutTableWidth \n" in {

                    test(new MagnitudeLog[FixedPoint](params = params))
                      .withAnnotations(annotations)
                      .runPeekPoke(c =>
                        new MagnitudeLogTester(
                          dut = c,
                          params = params,
                          sampleSize = sampleSize,
                          verbose = verbose,
                          random = random,
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
