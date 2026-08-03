package opera.windowing

import chisel3.RawModule
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import fixedpoint.FixedPoint
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.{AppLogger, StandaloneAXI4Block, StandaloneTLBlock}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule

private object WindowingApp {
  def parameters(args: Array[String]): ParseParameters.Parsed = {
    if (args.isEmpty) {
      AppLogger.warn("No custom configuration was specified.")
      AppLogger.info("Using default parameters for Windowing block.")
      (AddressSet(0x010000, 0xff), AddressSet(0x000000, 0x0fff), WindowingParams.fixed())
    } else {
      AppLogger.info("Applying custom configuration")
      ParseParameters.parseconfig(args(0)) match {
        case Left(parameters) => parameters
        case Right(_) =>
          AppLogger.error("Something went wrong when acquiring Windowing Parameters")
          throw new Exception("Invalid configuration")
      }
    }
  }

  def tlModule(
      csrAddress: AddressSet,
      ramAddress: AddressSet,
      params: WindowingParams[FixedPoint],
      beatBytes: Int
  )(implicit p: Parameters): WindowingTL[FixedPoint] with StandaloneTLBlock = LazyModule(
    new WindowingTL[FixedPoint](csrAddress, ramAddress, params, beatBytes)
      with StandaloneTLBlock {
        override def standaloneParams: TLBundleParameters = TLBundleParameters(
          addressBits = beatBytes * 8,
          dataBits = beatBytes * 8,
          sourceBits = 4,
          sinkBits = 1,
          sizeBits = 2,
          echoFields = Nil,
          requestFields = Nil,
          responseFields = Nil,
          hasBCE = false)

        override def dataBytes: Int = 4
      })

  def emit(targetDirectory: String, module: => RawModule): Unit = {
    (new ChiselStage).execute(
      Array("--target", "systemverilog"),
      Seq(
        ChiselGeneratorAnnotation(() => module),
        FirtoolOption("--disable-all-randomization"),
        FirtoolOption("--split-verilog"),
        FirtoolOption(s"--o=$targetDirectory")
      )
    )
  }
}

/**
 * AXI4App app sets up parameters for a windowing DSP block connected via AXI4,
 * instantiates the module, and invokes the Chisel build process to emit SystemVerilog RTL.
 *
 * It accepts an optional command-line argument to specify a custom configuration;
 * otherwise, it uses default parameters and logs this choice.
 *
 * Behavior:
 * - If no command-line arguments are supplied, default AddressSets and parameters are used.
 * - If an argument is given, it attempts to parse a configuration.
 * - Logs parameterization steps and errors with `AppLogger`.
 *
 * Example usage from command line:
 *
 *     $ sbt "project windowing; runMain windowing.AXI4App windowing/src/main/resources/parameters.json"
 *
 * @note Output files will be written to ./rtl/WindowingAXI4.
 */
object AXI4App extends App {
  implicit val p: Parameters = Parameters.empty

  private val beatBytes = 4
  private val blockParams = WindowingApp.parameters(args)
  private val windowingModule = LazyModule(
    new WindowingAXI4[FixedPoint](
      csrAddress = blockParams._1,
      ramAddress = blockParams._2,
      errAddress = Nil,
      params = blockParams._3,
      beatBytes = beatBytes
    ) with StandaloneAXI4Block {
      override def standaloneParams: AXI4BundleParameters = AXI4BundleParameters(
        addrBits = beatBytes * 8,
        dataBits = beatBytes * 8,
        idBits = 1
      )

      override def dataBytes: Int = 4
    }
  )

  WindowingApp.emit("./rtl/WindowingAXI4", windowingModule.module)
}

/**
 * TLApp app sets up parameters for a windowing DSP block connected via TileLink,
 * instantiates the module, and invokes the Chisel build process to emit SystemVerilog RTL.
 *
 * It accepts an optional command-line argument to specify a custom configuration;
 * otherwise, it uses default parameters and logs this choice.
 *
 * Behavior:
 * - If no command-line arguments are supplied, default AddressSets and parameters are used.
 * - If an argument is given, it attempts to parse a configuration.
 * - Logs parameterization steps and errors with `AppLogger`.
 *
 * Example usage from command line:
 *
 *     $ sbt "project windowing; runMain windowing.TLApp windowing/src/main/resources/parameters.json"
 *
 * @note Output files will be written to ./rtl/WindowingTL.
 */
object TLApp extends App {
  implicit val p: Parameters = Parameters.empty

  private val beatBytes = 4
  private val blockParams = WindowingApp.parameters(args)
  private val windowingModule =
    WindowingApp.tlModule(blockParams._1, blockParams._2, blockParams._3, beatBytes)

  WindowingApp.emit("./rtl/WindowingTL", windowingModule.module)
}
