package opera.cfar

import chisel3._
import chiseltest.ChiselScalatestTester
import dsptools.numbers.{BinaryRepresentation, Real}
import fixedpoint._
import freechips.rocketchip.amba.axi4.AXI4BundleParameters
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink.TLBundleParameters
import opera.common.{StandaloneAXI4Block, StandaloneTLBlock}
import opera.lis.LISType
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.LazyModule
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

private[cfar] object MemoryMappedCFARDutFactory {
  def axi4Dut[T <: Data: Real: BinaryRepresentation](
      address  : AddressSet,
      params   : CFARParams[T],
      beatBytes: Int
  )(implicit p: Parameters): CFARAXI4[T] with StandaloneAXI4Block =
    LazyModule(
      new CFARAXI4(address = address, params = params, beatBytes = beatBytes) with StandaloneAXI4Block {
        override def standaloneParams: AXI4BundleParameters =
          AXI4BundleParameters(addrBits = 32, dataBits = beatBytes * 8, idBits = 1)
        override def dataBytes: Int = math.ceil(params.inputType.getWidth.toDouble / 8).toInt
      }
    )

  def tlDut[T <: Data: Real: BinaryRepresentation](
      address  : AddressSet,
      params   : CFARParams[T],
      beatBytes: Int
  )(implicit p: Parameters): CFARTL[T] with StandaloneTLBlock =
    LazyModule(
      new CFARTL(address = address, params = params, beatBytes = beatBytes) with StandaloneTLBlock {
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
        override def dataBytes: Int = math.ceil(params.inputType.getWidth.toDouble / 8).toInt
      }
    )
}

abstract class MemoryMappedCFARSpec(
    suiteName: String,
    busName  : String,
    protected val beatBytes: Int,
) extends AnyFlatSpec with ChiselScalatestTester with TestUtils {
  behavior of suiteName

  implicit val p: Parameters = Parameters.empty
  protected val address: AddressSet = AddressSet(0x2000, 0xFF)

  private val fixedType = FixedPoint(16.W, 14.BP)
  private val caModes   = CFARModel.caModes
  private val gosModes  = CFARModel.gosModes
  private val rankPairs = Seq((2, 2), (2, 3))
  private val caWindows = Seq(
    CFARTestWindowCase(16, 2, 1),
    CFARTestWindowCase(32, 4, 1),
    CFARTestWindowCase(64, 8, 2)
  )
  private val staticReadyPattern         = Seq(true, true, false, true, true, false, true)
  private val realisticFftSize           = 1024
  private val realisticMaxReferenceCells = 32
  private val realisticReferenceCells    = 16
  private val realisticGuardCells        = 8
  private val realisticThresholdScale    = 1.5
  private val realisticData = realisticFftMagnitudeFrame(realisticFftSize, seed = 0x1024CFAFL)

  protected def runMemoryMappedCheck(params: CFARParams[FixedPoint], mmCheck: MemoryMappedCFARCheck): Unit

  private def passWhen(check: String, fields: (String, Any)*): String =
    "pass when:\n" + (Seq("check" -> check, "bus" -> busName, "beatBytes" -> beatBytes) ++ fields)
      .map { case (key, value) => s"\t\t$key = $value" }
      .mkString(",\n") + "\n"

  private def modeName(mode: CFARModel.CAMode): String =
    mode.name.replace(" ", "").replace("-", "")

  private def frame(
      name                       : String,
      fftSize                    : Int,
      config                     : MemoryMappedCFARRuntimeConfig,
      dataSeed                   : Long,
      readyPattern               : Seq[Boolean] = staticReadyPattern,
      randomSeed                 : Option[Long] = None,
      randomReadyValid           : Boolean = true,
      inputData                  : Option[Seq[Double]] = None,
      plotName                   : Option[String] = None
  ): MemoryMappedCFARFrameCase =
    MemoryMappedCFARFrameCase(
      name                        = name,
      fftSize                     = fftSize,
      config                      = config,
      dataSeed                    = dataSeed,
      readyPattern                = readyPattern,
      randomReadyValidSeed        = randomSeed,
      defaultRandomReadyValidSeed = dataSeed ^ 0x5eed5eedL,
      randomReadyValid            = randomReadyValid,
      inputData                   = inputData,
      plotName                    = plotName
    )

  private def paramsForCase(testCase: MemoryMappedCFARCase, dataType: FixedPoint = fixedType): CFARParams[FixedPoint] =
    paramsFor(
      dataType,
      maxFftSize        = testCase.maxFftSize,
      maxReferenceCells = testCase.maxReferenceCells,
      maxGuardCells     = testCase.maxGuardCells,
      cfarType          = testCase.cfarType,
      lisType           = testCase.lisType,
      sendCut           = testCase.sendCut,
      logMode           = testCase.logMode,
      runtimeLogMode    = testCase.runtimeLogMode,
      edgePolicy        = testCase.edgePolicy,
      runtimeEdgePolicy = testCase.runtimeEdgePolicy,
      retiming          = testCase.retiming,
      addPipeStages     = testCase.addPipeStages,
      mulPipeStages     = testCase.mulPipeStages
    )

  private def realisticFftMagnitudeFrame(size: Int, seed: Long): Vector[Double] = {
    require(size == realisticFftSize, s"realistic memory-mapped CFAR frame expects $realisticFftSize bins, got $size")
    val random = new Random(seed)
    val tones = Seq(
      6          -> 1.12,
      94         -> 0.98,
      122        -> 0.76,
      389        -> 1.05,
      (size - 7) -> 1.18
    )

    Vector.tabulate(size) { bin =>
      val slowFloor  = 0.026 + 0.008 * (1.0 + math.sin(2.0 * math.Pi * bin.toDouble / size.toDouble * 5.0))
      val noise      = random.nextDouble() * 0.026
      val toneEnergy = tones.map { case (toneBin, amplitude) =>
        val distance = math.abs(bin - toneBin)
        val circularDistance = math.min(distance, size - distance)
        circularDistance match {
          case 0 => amplitude
          case 1 => amplitude * 0.18
          case 2 => amplitude * 0.08
          case 3 => amplitude * 0.035
          case 4 => amplitude * 0.018
          case _ => 0.0
        }
      }.sum

      math.min(1.25, slowFloor + noise + toneEnergy)
    }
  }

  private def realisticConfig(
      mode          : CFARModel.CAMode,
      edgePolicy    : Int,
      orderRankLeft : Int = 1,
      orderRankRight: Int = 1
  ): MemoryMappedCFARRuntimeConfig =
    MemoryMappedCFARRuntimeConfig(
      mode           = mode,
      thresholdScale = realisticThresholdScale,
      referenceCells = realisticReferenceCells,
      guardCells     = realisticGuardCells,
      edgePolicy     = edgePolicy,
      orderRankLeft  = orderRankLeft,
      orderRankRight = orderRankRight
    )

  private def realisticFrame(
      name          : String,
      mode          : CFARModel.CAMode,
      edgePolicy    : Int,
      dataSeed      : Long,
      orderRankLeft : Int = 1,
      orderRankRight: Int = 1
  ): MemoryMappedCFARFrameCase =
    frame(
      name             = name,
      fftSize          = realisticFftSize,
      config           = realisticConfig(mode, edgePolicy, orderRankLeft, orderRankRight),
      dataSeed         = dataSeed,
      readyPattern     = Seq(true),
      randomReadyValid = false,
      inputData        = Some(realisticData),
      plotName         = Some(s"${busName}-${name}_gc_${realisticGuardCells}_rc_${realisticReferenceCells}")
    )

  caWindows.zipWithIndex.foreach { case (window, windowIndex) =>
    it should passWhen(
      "sweep CA-family memory-mapped frames",
      "family"         -> "CA",
      "maxFftSize"     -> window.fftSize,
      "referenceCells" -> window.referenceCells,
      "guardCells"     -> window.guardCells,
      "modes"          -> caModes.map(_.name).mkString(","),
      "edgePolicies"   -> allEdgePolicies.map(edgePolicyName).mkString(",")
    ) in {
      val testCase = MemoryMappedCFARCase(
        family            = "CA",
        maxFftSize        = window.fftSize,
        maxReferenceCells = window.referenceCells,
        maxGuardCells     = window.guardCells,
        runtimeEdgePolicy = true
      )
      val frames = for {
        (mode, modeIndex) <- caModes.zipWithIndex
        (edgePolicy, edgeIndex) <- allEdgePolicies.zipWithIndex
      } yield frame(
        name = s"ca-${window.fftSize}-${modeName(mode)}-${edgePolicyName(edgePolicy)}",
        fftSize = window.fftSize,
        config = MemoryMappedCFARRuntimeConfig(
          mode           = mode,
          thresholdScale = 1.0,
          referenceCells = window.referenceCells,
          guardCells     = window.guardCells,
          edgePolicy     = edgePolicy
        ),
        dataSeed = 0xCA0000L + windowIndex * 0x1000L + modeIndex * 0x100L + edgeIndex
      )

      runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(frames))
    }
  }

  it should passWhen(
    "use runtime active fft_size smaller than maxFftSize",
    "family"         -> "CA",
    "maxFftSize"     -> 64,
    "activeFftSize"  -> 16,
    "referenceCells" -> 2,
    "guardCells"     -> 1,
    "edgePolicy"     -> edgePolicyName(CFAREdgePolicy.WrapAroundFrame)
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "CA",
      maxFftSize        = 64,
      maxReferenceCells = 8,
      maxGuardCells     = 2,
      runtimeEdgePolicy = true
    )
    val activeFrame = frame(
      name             = "ca-active-size-16",
      fftSize          = 16,
      config           = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.ClassicalCA,
        thresholdScale = 1.0,
        referenceCells = 2,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame
      ),
      dataSeed = 0xAC7100L
    )

    runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(Seq(activeFrame)))
  }

  it should passWhen(
    "pack CA-family memory-mapped result without CUT",
    "family"         -> "CA",
    "sendCut"        -> false,
    "maxFftSize"     -> 32,
    "edgePolicy"     -> edgePolicyName(CFAREdgePolicy.OneSidedAverage)
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "CA",
      maxFftSize        = 32,
      maxReferenceCells = 4,
      maxGuardCells     = 2,
      sendCut           = false,
      edgePolicy        = CFAREdgePolicy.OneSidedAverage
    )
    val cutlessFrame = frame(
      name             = "ca-sendcut-false",
      fftSize          = 32,
      config           = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.GOCA,
        thresholdScale = 1.0,
        referenceCells = 4,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage
      ),
      dataSeed = 0xCA7E55L
    )

    runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(Seq(cutlessFrame)))
  }

  it should passWhen(
    "apply two-frame CA runtime CSR reconfiguration",
    "family"            -> "CA",
    "runtimeLogMode"    -> true,
    "runtimeEdgePolicy" -> true,
    "firstMode"         -> CFARModel.GOCA.name,
    "secondMode"        -> CFARModel.SOCA.name
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "CA",
      maxFftSize        = 32,
      maxReferenceCells = 4,
      maxGuardCells     = 2,
      runtimeLogMode    = true,
      runtimeEdgePolicy = true,
      addPipeStages     = 1,
      mulPipeStages     = 1
    )
    val first = frame(
      name = "ca-reconfig-first-linear-one-sided",
      fftSize = 32,
      config = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.GOCA,
        thresholdScale = 1.0,
        logMode        = false,
        referenceCells = 4,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage
      ),
      dataSeed = 0x2F1100L
    )
    val second = frame(
      name = "ca-reconfig-second-log-wrap",
      fftSize = 32,
      config = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.SOCA,
        thresholdScale = 1.5,
        logMode        = true,
        referenceCells = 2,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame
      ),
      dataSeed = 0x2F2200L
    )

    runMemoryMappedCheck(paramsForCase(testCase), TwoFrameReconfigCheck(first, second))
  }

  it should passWhen(
    "hold mid-frame CA CSR load pending until next frame",
    "family"      -> "CA",
    "edgePolicy"  -> edgePolicyName(CFAREdgePolicy.OneSidedAverage),
    "firstMode"   -> CFARModel.GOCA.name,
    "pendingMode" -> CFARModel.SOCA.name
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "CA",
      maxFftSize        = 32,
      maxReferenceCells = 4,
      maxGuardCells     = 2,
      edgePolicy        = CFAREdgePolicy.OneSidedAverage
    )
    val first = frame(
      name = "ca-midframe-first",
      fftSize = 32,
      config = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.GOCA,
        thresholdScale = 1.0,
        referenceCells = 4,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage
      ),
      dataSeed = 0xB01100L
    )
    val pending = frame(
      name = "ca-midframe-pending",
      fftSize = 32,
      config = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.SOCA,
        thresholdScale = 1.0,
        referenceCells = 2,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.OneSidedAverage
      ),
      dataSeed = 0xB02200L
    )
    val second = pending.copy(name = "ca-midframe-second", dataSeed = 0xB03300L)

    runMemoryMappedCheck(paramsForCase(testCase), MidFramePendingConfigCheck(first, pending, second, updateAfterAcceptedIndex = 1))
  }

  for {
    lisType <- LISType.all
    edgePolicy <- allEdgePolicies
  } {
    it should passWhen(
      "sweep GOS memory-mapped ranks and modes",
      "family"     -> "GOS",
      "lisType"    -> lisType,
      "edgePolicy" -> edgePolicyName(edgePolicy),
      "modes"      -> gosModes.map(_.name).mkString(","),
      "rankPairs"  -> rankPairs.map { case (left, right) => s"$left/$right" }.mkString(",")
    ) in {
      val testCase = MemoryMappedCFARCase(
        family            = "GOS",
        maxFftSize        = 16,
        maxReferenceCells = 4,
        maxGuardCells     = 2,
        cfarType          = CFARType.OrderedStatistic,
        lisType           = lisType,
        edgePolicy        = edgePolicy
      )
      val frames = for {
        (mode, modeIndex) <- gosModes.zipWithIndex
        ((leftRank, rightRank), rankIndex) <- rankPairs.zipWithIndex
      } yield frame(
        name = s"gos-$lisType-${modeName(mode)}-${edgePolicyName(edgePolicy)}-$leftRank-$rightRank",
        fftSize = 16,
        config = MemoryMappedCFARRuntimeConfig(
          mode           = mode,
          thresholdScale = 1.0,
          referenceCells = 4,
          guardCells     = 1,
          edgePolicy     = edgePolicy,
          orderRankLeft  = leftRank,
          orderRankRight = rightRank
        ),
        dataSeed = 0x605000L + lisType.length * 0x1000L + edgePolicy * 0x100L + modeIndex * 0x10L + rankIndex
      )

      runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(frames))
    }
  }

  it should passWhen(
    "pack GOS memory-mapped result without CUT",
    "family"     -> "GOS",
    "lisType"    -> LISType.CntBased,
    "sendCut"    -> false,
    "edgePolicy" -> edgePolicyName(CFAREdgePolicy.WrapAroundFrame)
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "GOS",
      maxFftSize        = 16,
      maxReferenceCells = 4,
      maxGuardCells     = 2,
      cfarType          = CFARType.OrderedStatistic,
      lisType           = LISType.CntBased,
      sendCut           = false,
      edgePolicy        = CFAREdgePolicy.WrapAroundFrame
    )
    val cutlessFrame = frame(
      name = "gos-sendcut-false",
      fftSize = 16,
      config = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.GOSSO,
        thresholdScale = 1.0,
        referenceCells = 4,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
        orderRankLeft  = 2,
        orderRankRight = 3
      ),
      dataSeed = 0x605C07L
    )

    runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(Seq(cutlessFrame)))
  }

  it should passWhen(
    "run one-reference GOS memory-mapped frame",
    "family"            -> "GOS",
    "lisType"           -> LISType.CntBased,
    "maxReferenceCells" -> 1,
    "maxGuardCells"     -> 1,
    "edgePolicy"        -> edgePolicyName(CFAREdgePolicy.WrapAroundFrame)
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "GOS",
      maxFftSize        = 8,
      maxReferenceCells = 1,
      maxGuardCells     = 1,
      cfarType          = CFARType.OrderedStatistic,
      lisType           = LISType.CntBased,
      edgePolicy        = CFAREdgePolicy.WrapAroundFrame
    )
    val oneReferenceFrame = frame(
      name = "gos-one-reference-wrap",
      fftSize = 8,
      config = MemoryMappedCFARRuntimeConfig(
        mode           = CFARModel.GOSCA,
        thresholdScale = 1.0,
        referenceCells = 1,
        guardCells     = 1,
        edgePolicy     = CFAREdgePolicy.WrapAroundFrame,
        orderRankLeft  = 1,
        orderRankRight = 1
      ),
      dataSeed = 0x605001L
    )

    runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(Seq(oneReferenceFrame)))
  }

  it should passWhen(
    s"stream realistic $realisticFftSize-point multi-tone-noise CA memory-mapped frames",
    "family"            -> "CA",
    "dataType"          -> "FixedPoint(16,14)",
    "maxFftSize"        -> realisticFftSize,
    "maxReferenceCells" -> realisticMaxReferenceCells,
    "referenceCells"    -> realisticReferenceCells,
    "guardCells"        -> realisticGuardCells,
    "thresholdScale"    -> realisticThresholdScale,
    "edgePolicies"      -> allEdgePolicies.map(edgePolicyName).mkString(",")
  ) in {
    val testCase = MemoryMappedCFARCase(
      family            = "CA",
      maxFftSize        = realisticFftSize,
      maxReferenceCells = realisticMaxReferenceCells,
      maxGuardCells     = realisticGuardCells,
      runtimeEdgePolicy = true
    )
    val frames = Seq(
      realisticFrame(
        name       = "realistic-ca-classical-suppress",
        mode       = CFARModel.ClassicalCA,
        edgePolicy = CFAREdgePolicy.SuppressEdges,
        dataSeed   = 0xCA102401L
      ),
      realisticFrame(
        name       = "realistic-ca-goca-one-sided",
        mode       = CFARModel.GOCA,
        edgePolicy = CFAREdgePolicy.OneSidedAverage,
        dataSeed   = 0xCA102402L
      ),
      realisticFrame(
        name       = "realistic-ca-soca-wrap",
        mode       = CFARModel.SOCA,
        edgePolicy = CFAREdgePolicy.WrapAroundFrame,
        dataSeed  = 0xCA102403L
      )
    )

    runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(frames))
  }

  private val realisticGosCases = Seq(
    (
      "realistic-gos-cntbased-ca-suppress",
      LISType.CntBased,
      CFARModel.GOSCA,
      CFAREdgePolicy.SuppressEdges,
      8,
      8,
      0x60510241L
    ),
    (
      "realistic-gos-regbased-go-one-sided",
      LISType.RegBased,
      CFARModel.GOSGO,
      CFAREdgePolicy.OneSidedAverage,
      8,
      10,
      0x60510242L
    ),
    (
      "realistic-gos-cntbased-so-wrap",
      LISType.CntBased,
      CFARModel.GOSSO,
      CFAREdgePolicy.WrapAroundFrame,
      10,
      8,
      0x60510243L
    )
  )

  realisticGosCases.foreach { case (label, lisType, mode, edgePolicy, leftRank, rightRank, dataSeed) =>
    it should passWhen(
      s"stream realistic $realisticFftSize-point multi-tone-noise GOS memory-mapped frame",
      "family"            -> "GOS",
      "dataType"          -> "FixedPoint(16,14)",
      "lisType"           -> lisType,
      "mode"              -> mode.name,
      "edgePolicy"        -> edgePolicyName(edgePolicy),
      "orderRanks"        -> s"$leftRank/$rightRank",
      "maxFftSize"        -> realisticFftSize,
      "maxReferenceCells" -> realisticMaxReferenceCells,
      "thresholdScale"    -> realisticThresholdScale
    ) in {
      val testCase = MemoryMappedCFARCase(
        family            = "GOS",
        maxFftSize        = realisticFftSize,
        maxReferenceCells = realisticMaxReferenceCells,
        maxGuardCells     = realisticGuardCells,
        cfarType          = CFARType.OrderedStatistic,
        lisType           = lisType,
        runtimeEdgePolicy = true
      )
      val frameCase = realisticFrame(
        name           = label,
        mode           = mode,
        edgePolicy     = edgePolicy,
        dataSeed       = dataSeed,
        orderRankLeft  = leftRank,
        orderRankRight = rightRank
      )

      runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(Seq(frameCase)))
    }
  }

  private val randomCases = Seq(
    (
      "ca-stream-one-sided",
      MemoryMappedCFARCase(
        family            = "CA",
        maxFftSize        = 32,
        maxReferenceCells = 4,
        maxGuardCells     = 2,
        edgePolicy        = CFAREdgePolicy.OneSidedAverage
      ),
      frame(
        name = "ca-random-stream-one-sided",
        fftSize = 32,
        config = MemoryMappedCFARRuntimeConfig(
          mode           = CFARModel.GOCA,
          thresholdScale = 1.0,
          referenceCells = 4,
          guardCells     = 1,
          edgePolicy     = CFAREdgePolicy.OneSidedAverage
        ),
        dataSeed   = 0xA5101L,
        randomSeed = Some(0xCA5101L)
      )
    ),
    (
      "ca-frame-wrap-pipeline",
      MemoryMappedCFARCase(
        family            = "CA",
        maxFftSize        = 32,
        maxReferenceCells = 4,
        maxGuardCells     = 2,
        edgePolicy        = CFAREdgePolicy.WrapAroundFrame,
        mulPipeStages     = 1
      ),
      frame(
        name = "ca-random-frame-wrap",
        fftSize = 32,
        config = MemoryMappedCFARRuntimeConfig(
          mode           = CFARModel.SOCA,
          thresholdScale = 1.0,
          referenceCells = 4,
          guardCells     = 1,
          edgePolicy     = CFAREdgePolicy.WrapAroundFrame
        ),
        dataSeed   = 0xA5102L,
        randomSeed = Some(0xCA5102L)
      )
    ),
    (
      "gos-cntbased-random",
      MemoryMappedCFARCase(
        family            = "GOS",
        maxFftSize        = 16,
        maxReferenceCells = 4,
        maxGuardCells     = 2,
        cfarType          = CFARType.OrderedStatistic,
        lisType           = LISType.CntBased,
        edgePolicy        = CFAREdgePolicy.SuppressEdges
      ),
      frame(
        name = "gos-cntbased-random",
        fftSize = 16,
        config = MemoryMappedCFARRuntimeConfig(
          mode           = CFARModel.GOSCA,
          thresholdScale = 1.0,
          referenceCells = 4,
          guardCells     = 1,
          edgePolicy     = CFAREdgePolicy.SuppressEdges,
          orderRankLeft  = 2,
          orderRankRight = 3
        ),
        dataSeed = 0x605CA1L,
        randomSeed = Some(0x605CA1L)
      )
    ),
    (
      "gos-regbased-random",
      MemoryMappedCFARCase(
        family            = "GOS",
        maxFftSize        = 16,
        maxReferenceCells = 4,
        maxGuardCells     = 2,
        cfarType          = CFARType.OrderedStatistic,
        lisType           = LISType.RegBased,
        edgePolicy        = CFAREdgePolicy.OneSidedAverage
      ),
      frame(
        name = "gos-regbased-random",
        fftSize = 16,
        config = MemoryMappedCFARRuntimeConfig(
          mode           = CFARModel.GOSGO,
          thresholdScale = 1.0,
          referenceCells = 4,
          guardCells     = 1,
          edgePolicy     = CFAREdgePolicy.OneSidedAverage,
          orderRankLeft  = 2,
          orderRankRight = 2
        ),
        dataSeed = 0x605CA2L,
        randomSeed = Some(0x605CA2L)
      )
    )
  )

  randomCases.foreach { case (label, testCase, randomFrame) =>
    it should passWhen(
      "preserve memory-mapped CFAR output with randomized ready-valid",
      "case"       -> label,
      "family"     -> testCase.family,
      "lisType"    -> testCase.lisType,
      "randomSeed" -> randomFrame.randomReadyValidSeed.get
    ) in {
      runMemoryMappedCheck(paramsForCase(testCase), SingleFrameSweepCheck(Seq(randomFrame)))
    }
  }
}
