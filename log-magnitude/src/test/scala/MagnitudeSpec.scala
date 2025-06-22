package opera.logmagnitude

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Magnitude"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes  = 4
  val sampleSize = 1024
  val verbose    = true
  val random     = true

  for (magType <- Seq(JPL)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (mulPipeRegs <- Seq(false)) {
        for (binaryGrowth <- Seq(0)) {
          for (binaryPoint <- Seq(10, 12, 14)) {
            for (binaryPointDiff <- Seq(0, 2, 4)) {
              // Parameters
              val params     = LogMagnitudeParams[FixedPoint](
                inputType    = DspComplex(FixedPoint(16.W, binaryPoint.BP)),
                outputType   = FixedPoint((16 + binaryPointDiff).W, (binaryPoint + binaryPointDiff).BP),
                magType      = magType,
                addPipeRegs  = addPipeRegs,
                mulPipeRegs  = mulPipeRegs,
                binaryGrowth = binaryGrowth,
                trimType     = Convergent
              )

              it should "pass when: \n" +
                s"\t\tmagType = $magType,\n" +
                s"\t\taddPipeRegs     = $addPipeRegs,\n" +
                s"\t\tmulPipeRegs     = $mulPipeRegs,\n" +
                s"\t\tbinaryGrowth    = $binaryGrowth,\n" +
                s"\t\tbinaryPoint     = $binaryPoint,\n" +
                s"\t\tbinaryPointDiff = $binaryPointDiff\n" in {

                test(new MagnitudeJPL[FixedPoint](params = params))
                  .withAnnotations(annotations)
                  .runPeekPoke(c =>
                    new MagnitudeTester(
                      dut = c,
                      params = params,
                      sampleSize = sampleSize,
                      verbose = verbose,
                      random = random
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
