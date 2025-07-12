package opera.logmagnitude

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeJPLSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MagnitudeJPL"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val sampleSize = 256
  val verbose    = true
  val random     = true
  val dataRandom = true

  for (magType <- Seq(JPL)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (inBinaryPoint <- Seq(10, 12, 14)) {
        for (outBinaryPoint <- Seq(10, 12, 14)) {
          for (inWholePart <- Seq(2, 3, 4)) {
            for (extendOut <- Seq(0, 1, 2)) {
              val outputWholePart = inWholePart + 2 + extendOut
              // Parameters
              val params = LogMagnitudeParams[FixedPoint](
                inputType   = DspComplex(FixedPoint((inWholePart + inBinaryPoint).W, inBinaryPoint.BP)),
                outputType  = FixedPoint((outputWholePart + outBinaryPoint).W, outBinaryPoint.BP),
                magType     = magType,
                addPipeRegs = addPipeRegs,
                trimType    = Convergent
              )

              it should "pass when: \n" +
                s"\t\tmagType         = $magType, \n" +
                s"\t\taddPipeRegs     = $addPipeRegs, \n" +
                s"\t\tinWholePart     = $inWholePart, \n" +
                s"\t\tinBinaryPoint   = $inBinaryPoint, \n" +
                s"\t\toutputWholePart = $outputWholePart, \n" +
                s"\t\toutBinaryPoint  = $outBinaryPoint \n " in {

                test(new MagnitudeJPL[FixedPoint](params = params))
                  .withAnnotations(annotations)
                  .runPeekPoke(c =>
                    new MagnitudeJPLTester(
                      dut        = c,
                      params     = params,
                      sampleSize = sampleSize,
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
