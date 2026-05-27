package opera.cfar

import _root_.circt.stage.{ChiselStage, FirtoolOption}
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import fixedpoint._
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import opera.lis.LISType
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

private object CFARAppConfig {
  type BlockParams = (AddressSet, CFARParams[FixedPoint], Int)

  val defaultAddress: AddressSet = AddressSet(0x2000, 0xFF)

  val defaultParams: CFARParams[FixedPoint] = CFARParams.fixed(
    inputType         = FixedPoint(16.W, 14.BP),
    thresholdType     = FixedPoint(16.W, 14.BP),
    scaleType         = FixedPoint(16.W, 14.BP),
    cfarType          = CFARType.CellAveraging,
    lisType           = LISType.CntBased,
    maxReferenceCells = 16,
    maxGuardCells     = 4,
    maxFftSize        = 1024,
    sendCut           = true,
    logMode           = false,
    runtimeLogMode    = false,
    edgePolicy        = CFAREdgePolicy.OneSidedAverage,
    runtimeEdgePolicy = false,
    retiming          = false,
    addPipeStages     = 0,
    mulPipeStages     = 0,
    minSRAMDepth      = 8
  )

  val defaultBeatBytes: Int = 4

  def blockParams(args: Array[String]): BlockParams =
    if (args.length == 0) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for CFAR block.")
      (defaultAddress, defaultParams, defaultBeatBytes)
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case Right(e) =>
          AppLogger.error(s"Something went wrong when acquiring CFAR Parameters: ${e.getMessage}")
          throw new Exception("Invalid configuration", e)
      }
    }
}

private object CFARApp {
  def emitAXI4(args: Array[String]): Unit = {
    implicit val p: Parameters = Parameters.empty
    val blockParams = CFARAppConfig.blockParams(args)
    val address     = blockParams._1
    val params      = blockParams._2
    val beatBytes   = blockParams._3

    val cfarModule = LazyModule(
      new CFARAXI4(address, params, beatBytes)
        with StandaloneAXI4Block {
        override def standaloneParams: AXI4BundleParameters =
          AXI4BundleParameters(
            addrBits = beatBytes * 8,
            dataBits = beatBytes * 8,
            idBits   = 1
          )
        override def dataBytes: Int = math.ceil(params.inputType.getWidth.toDouble / 8).toInt
      }
    )

    (new ChiselStage).execute(
      Array("--target", "systemverilog"),
      Seq(
        ChiselGeneratorAnnotation(() => cfarModule.module),
        FirtoolOption("--disable-all-randomization"),
        FirtoolOption("--split-verilog"),
        FirtoolOption("--o=./rtl/CFARAXI4")
      )
    )
  }

  def emitTL(args: Array[String]): Unit = {
    implicit val p: Parameters = Parameters.empty
    val blockParams = CFARAppConfig.blockParams(args)
    val address     = blockParams._1
    val params      = blockParams._2
    val beatBytes   = blockParams._3

    val cfarModule = LazyModule(
      new CFARTL(address, params, beatBytes)
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
        override def dataBytes: Int = math.ceil(params.inputType.getWidth.toDouble / 8).toInt
      }
    )

    (new ChiselStage).execute(
      Array("--target", "systemverilog"),
      Seq(
        ChiselGeneratorAnnotation(() => cfarModule.module),
        FirtoolOption("--disable-all-randomization"),
        FirtoolOption("--split-verilog"),
        FirtoolOption("--o=./rtl/CFARTL")
      )
    )
  }
}

object AXI4App extends App {
  CFARApp.emitAXI4(args)
}

object TLApp extends App {
  CFARApp.emitTL(args)
}
