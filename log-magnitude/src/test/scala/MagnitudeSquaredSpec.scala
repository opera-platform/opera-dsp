package opera.logmagnitude

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeSquaredSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Magnitude"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes   = 4
  val sampleSize  = 1024
  val verbose     = true
  val random      = true
  val inputWidth  = 16
  val binaryPoint = 10

  for (magType <- Seq(Squared)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (mulPipeRegs <- Seq(false, true)) {
        for (binaryGrowth <- Seq(binaryPoint)) {
          for (binaryPoint <- Seq(binaryPoint)) {
            for (binaryPointDiff <- Seq(0, 2, 4)) {
              // Parameters
              val outputWidth = inputWidth + (inputWidth - binaryPoint) + binaryPointDiff + 1
              val params     = LogMagnitudeParams[FixedPoint](
                inputType    = DspComplex(FixedPoint(inputWidth.W, binaryPoint.BP)),
                outputType   = FixedPoint(outputWidth.W, (binaryPoint + binaryPointDiff).BP),
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

                test(new MagnitudeSquared[FixedPoint](params = params))
                  .withAnnotations(annotations)
                  .runPeekPoke(c =>
                    new MagnitudeSquaredTester(
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
