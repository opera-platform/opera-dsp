package opera.fft

import chisel3.util.log2Up
import chiseltest.ChiselScalatestTester
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.{StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.scalatest.flatspec.AnyFlatSpec

import ModelUtils.RawComplex

private[fft] sealed trait MemoryMappedFFTCheck
private[fft] final case class StaticFrameCheck(inputData: Vector[RawComplex], expectedData: Vector[RawComplex], plotName: String) extends MemoryMappedFFTCheck
private[fft] case object RuntimeConfigCheck extends MemoryMappedFFTCheck
private[fft] case object OverflowCsrCheck extends MemoryMappedFFTCheck

private[fft] final case class MemoryMappedFFTCase(radix: SDFRadix, decimation: DecimationType, size: Int)

abstract class MemoryMappedFFTSpec(
    suiteName: String,
    busName  : String,
    protected val beatBytes: Int,
    staticFrameSeed: Long,
) extends AnyFlatSpec with ChiselScalatestTester with TestConfigSupport {
  behavior of suiteName

  implicit val p: Parameters = Parameters.empty
  protected val address: AddressSet = AddressSet(0x1000, 0xFF)
  protected def annotationsFor(params: FFTParams) = TestConfig.annotationsForFftSize(params.fftSize)
  
  protected def runMemoryMappedCheck(params: FFTParams, mmCheck: MemoryMappedFFTCheck): Unit
  private def passWhen(check: String, fields: (String, Any)*): String =
    TestUtils.passWhen((Seq("check" -> check, "beatBytes" -> beatBytes) ++ fields): _*)

  private val radices     = Seq(Radix2, Radix22)
  private val decimations = Seq(DIF, DIT)

  private val staticCases =
    for {
      radix      <- radices
      decimation <- decimations
      size       <- Seq(64, 256, 1024)
    } yield MemoryMappedFFTCase(radix, decimation, size)

  private val runtimeCases =
    for {
      radix      <- radices
      decimation <- decimations
    } yield MemoryMappedFFTCase(radix, decimation, size = 1024)

  private val overflowCases =
    for {
      radix      <- radices
      decimation <- decimations
      size       <- Seq(64, 256)
    } yield MemoryMappedFFTCase(radix, decimation, size)

  staticCases.foreach { config =>
    it should passWhen(
      "stream static FFT frame and preserve last",
      "sdfRadix"   -> config.radix.label,
      "decimation" -> config.decimation,
      "size"       -> config.size
    ) in {
      val params = FFTModelTestUtils.fftParams(config.radix, config.size, config.decimation)
      val pattern = InputPatterns.multiTonePattern(
        params.fftSize,
        baseAmplitudeRaw = BigInt(2048),
        noise            = Some(InputPatterns.FftNoise(maxAmplitudeRaw = 512, seed = staticFrameSeed + config.size)),
        label            = "multi-tone-noise"
      )
      val inputData    = FFTModelTestUtils.patternedDutInput(params, pattern, frames = 3)
      val expectedData = FFTModel(params, inputData).checkedFrame(params.fftSize)
      val plotName     = s"$busName-static-${config.radix.label}-${config.decimation}-${config.size}-multi-tone-noise"

      runMemoryMappedCheck(params, StaticFrameCheck(inputData, expectedData, plotName))
    }
  }

  runtimeCases.foreach { config =>
    it should passWhen(
      "apply runtime FFT CSR controls",
      "sdfRadix"   -> config.radix.label,
      "decimation" -> config.decimation,
      "maxSize"    -> config.size
    ) in {
      val params = FFTModelTestUtils
        .fftParams(config.radix, config.size, config.decimation)
        .copy(runTime = true, divBy2Reg = true, directionReg = true)

      runMemoryMappedCheck(params, RuntimeConfigCheck)
    }
  }

  overflowCases.foreach { config =>
    it should passWhen(
      "report sticky FFT overflow CSR status",
      "sdfRadix"   -> config.radix.label,
      "decimation" -> config.decimation,
      "size"       -> config.size
    ) in {
      val params = FFTModelTestUtils
        .fftParams(config.radix, config.size, config.decimation, dataWidth = 8, binPoint = 6, twiddleWidth = 8)
        .copy(divBy2 = Seq.fill(log2Up(config.size))(false), overflowReg = true)

      runMemoryMappedCheck(params, OverflowCsrCheck)
    }
  }
}

object MemoryMappedFFTSpecUtils {
  def axi4Dut(address: AddressSet, params: FFTParams, beatBytes: Int)(implicit p: Parameters): FFTAXI4 with StandaloneAXI4Block =
    LazyModule(
      new FFTAXI4(address = address, params = params, beatBytes = beatBytes) with StandaloneAXI4Block {
        override def standaloneParams: AXI4BundleParameters =
          AXI4BundleParameters(addrBits = 32, dataBits = beatBytes * 8, idBits = 1)
        override def dataBytes: Int = math.ceil(params.inDataType.getWidth.toDouble / 8).toInt
      }
    )

  def tlDut(address: AddressSet, params: FFTParams, beatBytes: Int)(implicit p: Parameters): FFTTL with StandaloneTLBlock =
    LazyModule(
      new FFTTL(address = address, params = params, beatBytes = beatBytes) with StandaloneTLBlock {
        override def standaloneParams: TLBundleParameters =
          TLBundleParameters(
            addressBits    = 32,
            dataBits       = beatBytes * 8,
            sourceBits     = 1,
            sinkBits       = 1,
            sizeBits       = 2,
            echoFields     = Nil,
            requestFields  = Nil,
            responseFields = Nil,
            hasBCE         = false
          )
        override def dataBytes: Int = math.ceil(params.inDataType.getWidth.toDouble / 8).toInt
      }
    )
}
