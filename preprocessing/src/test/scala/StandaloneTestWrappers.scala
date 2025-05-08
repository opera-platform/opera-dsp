package preprocessing

import dspblocks.DspBlock
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.diplomacy.bundlebridge.{BundleBridgeSink, BundleBridgeSource}
import org.chipsalliance.diplomacy.lazymodule.InModuleBody

// AXI4StreamBlock Standalone wrapper for test
trait TestAXI4StreamBlock extends AXI4StreamBlock {
  def dataBytes = 4

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = dataBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode := AXI4StreamBuffer(2) :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = dataBytes)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

// AXI4 DspBlock Standalone wrapper for test
trait TestStandaloneAXI4Block
  extends DspBlock[
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle
  ] {
  def dataBytes = 2
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  // AXI4
  val ioMem = mem.map {
    m => {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
      m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }
  }

  // AXI4Stream nodes
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = dataBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode := AXI4StreamBuffer(2) :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = dataBytes)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}