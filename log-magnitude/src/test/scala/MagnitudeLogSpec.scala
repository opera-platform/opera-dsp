package opera.logmagnitude

import chisel3.fromIntToWidth
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class MagnitudeLogSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Magnitude"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes   = 4
  val sampleSize  = 32
  val verbose     = true
  val random      = false
  val dataRandom  = false
  val inputWidth  = 16
  val binaryPoint = 10

  for (magType <- Seq(Log)) {
    for (addPipeRegs <- Seq(false)) {
      for (mulPipeRegs <- Seq(false)) {
        for (binaryGrowth <- Seq(binaryPoint)) {
          for (binaryPoint <- Seq(binaryPoint)) {
            for (binaryPointDiff <- Seq(0)) {
              // Parameters
              val params     = LogMagnitudeParams[FixedPoint](
                inputType    = DspComplex(FixedPoint(inputWidth.W, binaryPoint.BP)),
                realType     = Some(FixedPoint(inputWidth.W, binaryPoint.BP)),
                outputType   = FixedPoint(inputWidth.W, (binaryPoint).BP),
                logType      = Some(FixedPoint((binaryPoint + 1).W, binaryPoint.BP)),
                lutTableSize = 4,
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
