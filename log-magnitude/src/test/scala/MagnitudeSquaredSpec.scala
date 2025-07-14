package opera.logmagnitude

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeSquaredSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MagnitudeSquared"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val sampleSize  = 256
  val verbose     = true
  val random      = true
  val dataRandom  = true

  for (magType <- Seq(Squared)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (mulPipeRegs <- Seq(false, true)) {
        for (inBinaryPoint <- Seq(6, 10, 14)) {
          for (outBinaryPoint <- Seq(6, 10, 14)) {
            for (inWholePart <- Seq(2, 3, 4)) {
              for (extendOut <- Seq(0, 1, 2)) {
                val outWholePart = 2 * inWholePart + 1 + extendOut
                // Parameters
                val params = LogMagnitudeParams[FixedPoint](
                  inputType    = DspComplex(FixedPoint((inWholePart + inBinaryPoint).W, inBinaryPoint.BP)),
                  outputType   = FixedPoint((outWholePart + outBinaryPoint).W, outBinaryPoint.BP),
                  magType      = magType,
                  addPipeRegs  = addPipeRegs,
                  mulPipeRegs  = mulPipeRegs,
                  trimType     = Convergent
                )

                it should "pass when: \n" +
                  s"\t\tmagType        = $magType,\n" +
                  s"\t\taddPipeRegs    = $addPipeRegs, \n" +
                  s"\t\tmulPipeRegs    = $mulPipeRegs, \n" +
                  s"\t\tinWholePart    = $inWholePart, \n" +
                  s"\t\tinBinaryPoint  = $inBinaryPoint, \n" +
                  s"\t\toutWholePart   = $outWholePart, \n" +
                  s"\t\toutBinaryPoint = $outBinaryPoint \n" in {

                  test(new MagnitudeSquared[FixedPoint](params = params))
                    .withAnnotations(annotations)
                    .runPeekPoke(c =>
                      new MagnitudeSquaredTester(
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
}
