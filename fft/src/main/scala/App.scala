package opera.fft

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import dsptools._
import dsptools.numbers.DspComplex
import fixedpoint._
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

private object FFTAppConfig {
  type BlockParams = (AddressSet, FFTParams, Int)

  val defaultAddress: AddressSet = AddressSet(0x500, 0xFF)

  val defaultParams: FFTParams = FFTParams(
    inDataType       = DspComplex(FixedPoint(16.W, 14.BP)),
    twiddleType      = DspComplex(FixedPoint(16.W, 14.BP)),
    fftSize          = 1024,
    decimation       = DIF,
    sdfRadix         = Radix22,
    growEnable       = Seq.empty,
    runTime          = true,
    divBy2           = Seq.empty,
    divBy2Reg        = true,
    overflowReg      = true,
    trimType         = RoundHalfUp,
    numAddPipes      = 1,
    numMulPipes      = 1,
    direction        = true,
    directionReg     = true,
    use4Muls         = false,
    useBitReverse    = true,
    minSRAMdepth     = 8,
    singlePortSRAM   = false,
    stageTrimTypes   = Seq.empty,
    twiddleTrimTypes = Seq.empty
  )

  val defaultBeatBytes: Int = 4

  def blockParams(args: Array[String]): BlockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for FFT block.")
      (defaultAddress, defaultParams, defaultBeatBytes)
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case Right(e) =>
          AppLogger.error(s"Something went wrong when acquiring FFT Parameters: ${e.getMessage}")
          throw new Exception("Invalid configuration", e)
      }
    }
}

private object FFTApp {
  def emitAXI4(args: Array[String]): Unit = {
    implicit val p: Parameters = Parameters.empty
    val blockParams = FFTAppConfig.blockParams(args)
    val address     = blockParams._1
    val params      = blockParams._2
    val beatBytes   = blockParams._3

    val fftModule = LazyModule(
      new FFTAXI4(address, params, beatBytes)
        with StandaloneAXI4Block {
        override def standaloneParams: AXI4BundleParameters =
          AXI4BundleParameters(
            addrBits = beatBytes * 8,
            dataBits = beatBytes * 8,
            idBits   = 1
          )
        override def dataBytes: Int = math.ceil(params.inDataType.getWidth.toDouble / 8).toInt
      }
    )

    (new ChiselStage).execute(
      Array("--target", "systemverilog"),
      Seq(
        ChiselGeneratorAnnotation(() => fftModule.module),
        FirtoolOption("--disable-all-randomization"),
        FirtoolOption("--split-verilog"),
        FirtoolOption("--o=./rtl/FFTAXI4")
      )
    )
  }

  def emitTL(args: Array[String]): Unit = {
    implicit val p: Parameters = Parameters.empty
    val blockParams = FFTAppConfig.blockParams(args)
    val address = blockParams._1
    val params = blockParams._2
    val beatBytes = blockParams._3

    val fftModule = LazyModule(
      new FFTTL(address, params, beatBytes)
        with StandaloneTLBlock {
        override def standaloneParams: TLBundleParameters =
          TLBundleParameters(
            addressBits    = beatBytes * 8,
            dataBits       = beatBytes * 8,
            sourceBits     = 4,
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

    (new ChiselStage).execute(
      Array("--target", "systemverilog"),
      Seq(
        ChiselGeneratorAnnotation(() => fftModule.module),
        FirtoolOption("--disable-all-randomization"),
        FirtoolOption("--split-verilog"),
        FirtoolOption("--o=./rtl/FFTTL")
      )
    )
  }
}

object AXI4App extends App {
  FFTApp.emitAXI4(args)
}

object TLApp extends App {
  FFTApp.emitTL(args)
}
