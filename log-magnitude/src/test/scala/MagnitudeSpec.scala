package opera.logmagnitude

import chisel3.{Module, fromIntToWidth}
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
  val random     = false

  for (magType <- Seq(JPL)) {
    for (addPipeRegs <- Seq(false)) {
      for (mulPipeRegs <- Seq(false)) {
        for (binaryGrowth <- Seq(0)) {
          // Parameters
          val params = LogMagnitudeParams[FixedPoint](
            inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
            outputType   = FixedPoint(16.W, 14.BP),
            magType      = magType,
            addPipeRegs  = addPipeRegs,
            mulPipeRegs  = mulPipeRegs,
            binaryGrowth = binaryGrowth,
            trimType     = Convergent
          )

          it should "pass when: \n" +
            s"\t\tmagType = $magType,\n" +
            s"\t\taddPipeRegs    = $addPipeRegs,\n" +
            s"\t\tmulPipeRegs    = $mulPipeRegs,\n" +
            s"\t\tbinaryGrowth   = $binaryGrowth,\n"in {

            test(new MagnitudeJPL[FixedPoint](params = params))
              .withAnnotations(annotations)
              .runPeekPoke(c =>
                new MagnitudeTester(
                  dut       = c,
                  params    = params,
                  sampleSize = sampleSize,
                  verbose   = verbose,
                  random    = random
                )
              )
          }
        }
      }
    }
  }
}
