package preprocessing

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.diplomacy.lazymodule._
import scala.language.postfixOps

// Default path is: "preprocessing/src/main/resources/parameters.json"
// runMain preprocessing.*App "preprocessing/src/main/resources/parameters.json"
object AXI4App extends App {
  implicit val p: Parameters = Parameters.empty
  val baseAddress = 0x500
  val params: BlockParameters =
    if (args.length == 0) {
      DMALogger.warn("No custom configuration was specified.")
      DMALogger.info("Using default parameters for PreProcessing block.")
      BlockParameters()
    } else {
      DMALogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ => {
          DMALogger.error("Something went wrong when acquiring DMA Parameters")
          throw new Exception("Invalid configuration")
        }
      }
    }

  private val PreProcessingModule = LazyModule(
    new PreProcessingAXI4(AddressSet(baseAddress, 0xff), params, _beatBytes = 4)
      with PreProcessingAXI4Standalone
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => PreProcessingModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./verilog"))
  )
}

object TLApp extends App {
  implicit val p: Parameters = Parameters.empty
  val baseAddress = 0x500
  val params: BlockParameters =
    if (args.length == 0) {
      DMALogger.warn("No custom configuration was specified.")
      DMALogger.info("Using default parameters for PreProcessing block.")
      BlockParameters()
    } else {
      DMALogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(x) => x
        case _ => {
          DMALogger.error("Something went wrong when acquiring DMA Parameters")
          throw new Exception("Invalid configuration")
        }
      }
    }

  private val PreProcessingModule = LazyModule(
    new PreProcessingTL(AddressSet(baseAddress, 0xff), params, _beatBytes = 4)
      with PreProcessingTLStandalone
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => PreProcessingModule.module),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./verilog"))
  )
}
