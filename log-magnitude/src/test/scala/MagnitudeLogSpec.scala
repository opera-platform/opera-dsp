package opera.logmagnitude

import chisel3.fromIntToWidth
import chisel3.util.log2Ceil
import chiseltest.{ChiselScalatestTester, VerilatorBackendAnnotation, WriteVcdAnnotation}
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

// TODO: When using LUT size larger then binarypoint size, expand log type with 2 + logBinaryPoint instead of 1 + logBinaryPoint
class MagnitudeLogSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Magnitude"

  implicit val p: Parameters = Parameters.empty
  val annotations = Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)

  val beatBytes   = 4
  val sampleSize  = 1024
  val verbose     = true
  val random      = true
  val dataRandom  = true

  for (magType <- Seq(Log)) {
    for (addPipeRegs <- Seq(false, true)) {
      for (lutTableSize <- Seq(2, 4, 8, 10)) {
        for (binaryPoint <- Seq(6, 8, 10, 12)) {
          for (binaryLogPoint <- Seq(4, 6, 8, 10)) {
            val inputWidth = binaryPoint + log2Ceil(binaryPoint) + 1
            // Parameters
            val params = LogMagnitudeParams[FixedPoint](
              inputType    = DspComplex(FixedPoint(inputWidth.W, binaryPoint.BP)),
              realType     = Some(FixedPoint(inputWidth.W, binaryPoint.BP)),
              outputType   = FixedPoint(inputWidth.W, binaryPoint.BP),
              logType      = Some(FixedPoint((binaryLogPoint + 2).W, binaryLogPoint.BP)),
              lutTableSize = lutTableSize,
              magType      = magType,
              addPipeRegs  = addPipeRegs,
              mulPipeRegs  = false,
              binaryGrowth = 0,
              trimType     = Convergent
            )

            it should "pass when: \n" +
              s"\t\tmagType         = $magType,\n" +
              s"\t\taddPipeRegs     = $addPipeRegs,\n" +
              s"\t\tlutTableSize    = $lutTableSize,\n" +
              s"\t\tbinaryPoint     = $binaryPoint,\n" +
              s"\t\tbinaryLogPoint  = $binaryLogPoint,\n" in {

              test(new MagnitudeLog[FixedPoint](params = params))
                .withAnnotations(annotations)
                .runPeekPoke(c =>
                  new MagnitudeLogTester(
                    dut = c,
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
