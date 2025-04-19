package preprocessing

import dspblocks._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.resources._
import org.chipsalliance.diplomacy.bundlebridge._
import org.chipsalliance.diplomacy.lazymodule._

class PreProcessingTL(
  address:    AddressSet,
  params:     BlockParameters,
  _beatBytes: Int = 4
)(implicit p: Parameters)
  extends PreProcessing[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](
    params,
    _beatBytes
  ) with TLDspBlock with TLHasCSR {

  val device: SimpleDevice = new SimpleDevice("TLPreProcessing",  Seq("opera-platform", "TLPreProcessing")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem: Some[TLRegisterNode] = Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = _beatBytes))
}

trait PreProcessingTLStandalone extends PreProcessingTL {
  def standaloneParams =
    TLBundleParameters(
      addressBits = 32,
      dataBits = 32,
      sourceBits = 1,
      sinkBits = 1,
      sizeBits = 1,
      echoFields = Seq(),
      requestFields = Seq(),
      responseFields = Seq(),
      hasBCE = false
    )
  val ioMem = mem.map { m =>
  {
    val ioMemNode = BundleBridgeSource(() => TLBundle(standaloneParams))
    m := BundleBridgeToTL(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("bundleBridgeToTL")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }
  }

  val ioInNode  = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) :=
    ioInNode

  val in  = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO()  }
}
