package opera.fft

import chisel3._
import chiseltest._
import dsptools._
import dsptools.numbers.{Convergent, DspComplex}
import fixedpoint._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * SDFStageSpec defines the shared SDF stage test matrix.
 *
 * It builds radix stage DUTs, generates fixed-point input patterns, and checks
 * each configuration against FFTStageModel.
 *
 * @param stageName          Name shown in ScalaTest output.
 * @param strictPatternSeed  Seed used for strict overflow input patterns.
 * @tparam D                 DUT module type.
 */
abstract class SDFStageSpec[D <: Module](
    stageName        : String,
    strictPatternSeed: Long,
) extends AnyFlatSpec
    with ChiselScalatestTester
    with TestConfigSupport {
  behavior of stageName

  private def annotations = TestConfig.annotations
  private val defaultDataWidth = 24
  private val defaultBinPoint = 14

  protected def makeDut(params: RadixParams): D
  protected def dutIO(dut: D): RadixIO
  protected def makeModel(params: RadixParams): FFTStageModel = new FFTStageModel(params)

  private type InputPatternFactory = (FFTStageModel.FixedFormat, Int) => Vector[FFTStageModel.RawComplex]

  private val safeCornerPattern: InputPatternFactory =
    (format, stageSize) => SDFStageInputPatterns.safeCornerPattern(format, stageSize)
  private val overflowCornerPattern: InputPatternFactory =
    (format, stageSize) => SDFStageInputPatterns.overflowPattern(format, stageSize)
  private val strictOverflowPattern: InputPatternFactory =
    (format, stageSize) => SDFStageInputPatterns.strictPattern(format, stageSize, strictPatternSeed, frames = 2, includeOverflow = true)
  private val randomPattern: InputPatternFactory =
    (format, stageSize) => SDFStageInputPatterns.seededPattern(format, stageSize, seed = 0x5eed2026L, frames = 4)

  private case class SDFStageConfiguration(
      stageSize    : Int,
      decimation   : DecimationType,
      addPipeRegs  : Int = 1,
      mulPipeRegs  : Int = 1,
      growEnable   : Boolean = true,
      divBy2       : Boolean = false,
      divBy2Reg    : Boolean = false,
      bufferAsMem  : Boolean = false,
      singlePortMem: Boolean = false,
      dataWidth    : Int = defaultDataWidth,
      binPoint     : Int = defaultBinPoint,
      trimType     : TrimType = Convergent,
  )

  private case class StageFixedFormat(dataWidth: Int, binPoint: Int)

  private case class FixedPointScenario(
      name                   : String,
      growEnable             : Boolean,
      divBy2                 : Boolean = false,
      divBy2Reg              : Boolean = false,
      divBy2RegValue         : Boolean = false,
      inputPattern           : InputPatternFactory = safeCornerPattern,
      requireOverflowCoverage: Boolean = false,
      divBy2RegControl       : Option[Int => Boolean] = None,
      useRandomPattern       : Boolean = false,
  )

  private case class SDFStageTestConfiguration(
      check                  : String,
      stage                  : SDFStageConfiguration,
      inputPattern           : InputPatternFactory,
      scenario               : String = "",
      divBy2RegValue         : Boolean = false,
      requireOverflowCoverage: Boolean = false,
      divBy2RegControl       : Option[Int => Boolean] = None,
      idleOnly               : Boolean = false,
      resetOnly              : Boolean = false,
  )

  private val stageSizeSeq     = Seq(4, 8, 16)
  private val decimationSeq    = Seq(DIF, DIT)
  private val pipeRegsSeq      = Seq((0, 0), (1, 0), (0, 1), (1, 1), (2, 2))
  private val trimTypeSeq = Seq[TrimType](
    RoundDown,
    RoundUp,
    RoundTowardsZero,
    RoundTowardsInfinity,
    RoundHalfDown,
    RoundHalfUp,
    RoundHalfTowardsZero,
    RoundHalfTowardsInfinity,
    RoundHalfToEven,
    RoundHalfToOdd,
  )
  private val singlePortMemSeq = Seq(false, true)

  private val fixedPointFormatSeq = Seq(
    StageFixedFormat(dataWidth = 8, binPoint = 4),
    StageFixedFormat(dataWidth = 12, binPoint = 8),
    StageFixedFormat(dataWidth = 16, binPoint = 10),
    StageFixedFormat(dataWidth = 16, binPoint = 14),
  )

  private val fixedPointScenarioSeq = Seq(
    FixedPointScenario(name = "grow enabled", growEnable = true, useRandomPattern = true),
    FixedPointScenario(name = "static divide-by-2", growEnable = false, divBy2 = true, inputPattern = strictOverflowPattern, requireOverflowCoverage = true),
    FixedPointScenario(name = "no divide-by-2", growEnable = false, inputPattern = strictOverflowPattern, requireOverflowCoverage = true),
    FixedPointScenario(name = "overflow without divide-by-2", growEnable = false, inputPattern = overflowCornerPattern, requireOverflowCoverage = true),
    FixedPointScenario(
      name                    = "runtime divide-by-2",
      growEnable              = false,
      divBy2Reg               = true,
      divBy2RegValue          = true,
      inputPattern            = strictOverflowPattern,
      requireOverflowCoverage = true,
      divBy2RegControl        = Some(cycle => cycle % 4 == 0 || cycle % 4 == 1)
    ),
  )

  private def stageParams(config: SDFStageConfiguration): RadixParams = {
    val dataType = DspComplex(FixedPoint(config.dataWidth.W, config.binPoint.BP))
    val outDataType =
      if (config.growEnable) DspComplex(FixedPoint((config.dataWidth + 1).W, config.binPoint.BP)) else dataType
    RadixParams(
      inDataType    = dataType,
      outDataType   = outDataType,
      twiddleType   = DspComplex(FixedPoint(16.W, 14.BP)),
      stageSize     = config.stageSize,
      decimation    = config.decimation,
      overflowReg   = true,
      divBy2Reg     = config.divBy2Reg,
      divBy2        = config.divBy2,
      growEnable    = config.growEnable,
      latency       = 2 * config.addPipeRegs + config.mulPipeRegs,
      addPipeRegs   = config.addPipeRegs,
      mulPipeRegs   = config.mulPipeRegs,
      dspMul4       = false,
      delay         = config.stageSize / 2,
      bufferAsMem   = config.bufferAsMem,
      singlePortMem = config.singlePortMem,
      trimType      = config.trimType,
    )
  }

  private def runStage(config: SDFStageTestConfiguration): Unit = {
    val params = stageParams(config.stage)
    test(makeDut(params))
      .withAnnotations(annotations)
      .runPeekPoke { dut =>
        new SDFStageDutTester(
          dut,
          dutIO(dut),
          params,
          makeModel(params),
          stageName,
          config.inputPattern,
          config.divBy2RegValue,
          config.requireOverflowCoverage,
          config.divBy2RegControl,
        )
      }
  }

  private def runIdle(config: SDFStageConfiguration): Unit = {
    val params = stageParams(config)
    test(makeDut(params))
      .withAnnotations(annotations)
      .runPeekPoke(dut => new SDFStageIdleDutTester(dut, dutIO(dut), params, makeModel(params).counterInit))
  }

  private def runReset(config: SDFStageConfiguration): Unit = {
    val params = stageParams(config)
    test(makeDut(params))
      .withAnnotations(annotations)
      .runPeekPoke(dut => new SDFStageResetDutTester(dut, dutIO(dut), params, makeModel(params), stageName))
  }

  private def register(config: SDFStageTestConfiguration): Unit = {
    it should TestUtils.passWhen(titleFields(config): _*) in {
      if (config.idleOnly) runIdle(config.stage)
      else if (config.resetOnly) runReset(config.stage)
      else runStage(config)
    }
  }

  private def titleFields(config: SDFStageTestConfiguration): Seq[(String, Any)] = {
    val stage = config.stage
    Seq(
      "check"      -> config.check,
      "stageSize"  -> stage.stageSize,
      "decimation" -> stage.decimation,
    ) ++ Seq(
      Option.when(config.scenario.nonEmpty)("scenario" -> config.scenario),
      Option.when(stage.addPipeRegs != 1)("addPipeRegs" -> stage.addPipeRegs),
      Option.when(stage.mulPipeRegs != 1)("mulPipeRegs" -> stage.mulPipeRegs),
      Option.when(!stage.growEnable)("growEnable" -> stage.growEnable),
      Option.when(stage.divBy2)("divBy2" -> stage.divBy2),
      Option.when(stage.divBy2Reg)("divBy2Reg" -> stage.divBy2Reg),
      Option.when(stage.bufferAsMem)("bufferAsMem" -> stage.bufferAsMem),
      Option.when(stage.bufferAsMem)("singlePortMem" -> stage.singlePortMem),
      Option.when(stage.dataWidth != defaultDataWidth)("dataWidth" -> stage.dataWidth),
      Option.when(stage.binPoint != defaultBinPoint)("binaryPoint" -> stage.binPoint),
      Option.when(stage.trimType != Convergent)("trimType" -> stage.trimType),
    ).flatten
  }

  // Checks the baseline stage by feeding deterministic random data and comparing every valid output to the scalar model.
  private val baselineConfigs: Iterator[SDFStageTestConfiguration] = for {
    stageSize  <- stageSizeSeq.iterator
    decimation <- decimationSeq.iterator
  } yield SDFStageTestConfiguration(
    check = "stage model",
    stage = SDFStageConfiguration(stageSize = stageSize, decimation = decimation),
    inputPattern = randomPattern,
  )

  // Checks pipeline alignment by sweeping add/mul registers while the tester compares delayed model outputs on random data.
  private val pipelineConfigs: Iterator[SDFStageTestConfiguration] = for {
    (addPipeRegs, mulPipeRegs) <- pipeRegsSeq.iterator
    decimation                 <- decimationSeq.iterator
  } yield SDFStageTestConfiguration(
    check = "pipeline alignment",
    stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, addPipeRegs = addPipeRegs, mulPipeRegs = mulPipeRegs),
    inputPattern = randomPattern,
  )

  // Checks bit-accurate raw vectors by replaying deterministic seeds through the DUT and model.
  private val seededConfigs: Iterator[SDFStageTestConfiguration] = for {
    stageSize  <- stageSizeSeq.iterator
    decimation <- decimationSeq.iterator
  } yield SDFStageTestConfiguration(
    check = "seeded bit-accurate raw vectors",
    stage = SDFStageConfiguration(stageSize = stageSize, decimation = decimation, growEnable = false, divBy2 = false, dataWidth = 12, binPoint = 8),
    inputPattern = randomPattern,
  )

  // Checks non-default trim modes by forcing divide-by-2 overflow cases through alternate rounding.
  private val trimConfigs: Iterator[SDFStageTestConfiguration] = for {
    trimType   <- trimTypeSeq.iterator
    decimation <- decimationSeq.iterator
  } yield SDFStageTestConfiguration(
    check = "non-default divide-by-2 trim type",
    stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, growEnable = false, divBy2 = true, dataWidth = 12, binPoint = 8, trimType = trimType),
    inputPattern = strictOverflowPattern,
    requireOverflowCoverage = true,
  )

  // Checks fixed-point formats and scaling modes by combining widths, binary points, and overflow patterns.
  private val fixedPointConfigs: Iterator[SDFStageTestConfiguration] = for {
    format     <- fixedPointFormatSeq.iterator
    scenario   <- fixedPointScenarioSeq.iterator
    decimation <- decimationSeq.iterator
  } yield {
    val inputPattern = if (scenario.useRandomPattern) randomPattern else scenario.inputPattern
    SDFStageTestConfiguration(
      check = "bit-accurate fixed-point model",
      stage = SDFStageConfiguration(
        stageSize  = 8,
        decimation = decimation,
        growEnable = scenario.growEnable,
        divBy2     = scenario.divBy2,
        divBy2Reg  = scenario.divBy2Reg,
        dataWidth  = format.dataWidth,
        binPoint   = format.binPoint,
      ),
      scenario = scenario.name,
      divBy2RegValue = scenario.divBy2RegValue,
      inputPattern = inputPattern,
      requireOverflowCoverage = scenario.requireOverflowCoverage,
      divBy2RegControl = scenario.divBy2RegControl,
    )
  }

  // Checks control-side behavior by mixing idle, div-by-2, runtime control, and overflow flag scenarios.
  private val controlConfigs: Iterator[SDFStageTestConfiguration] = for {
    decimation <- decimationSeq.iterator
    config <- Seq(
      SDFStageTestConfiguration(
        check = "idle counter and output enable",
        stage = SDFStageConfiguration(stageSize = 8, decimation = decimation),
        inputPattern = safeCornerPattern,
        idleOnly = true,
      ),
      SDFStageTestConfiguration(
        check = "divide-by-2 scaling",
        stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, growEnable = false, divBy2 = true, binPoint = 8),
        inputPattern = strictOverflowPattern,
        requireOverflowCoverage = true,
      ),
      SDFStageTestConfiguration(
        check = "runtime divide-by-2 control",
        stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, growEnable = false, divBy2Reg = true, binPoint = 8),
        divBy2RegValue = true,
        inputPattern = strictOverflowPattern,
        requireOverflowCoverage = true,
        divBy2RegControl = Some(cycle => cycle % 4 == 0 || cycle % 4 == 1),
      ),
      SDFStageTestConfiguration(
        check = "overflow asserted",
        stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, growEnable = false, dataWidth = 5, binPoint = 1),
        inputPattern = overflowCornerPattern,
        requireOverflowCoverage = true,
      ),
      SDFStageTestConfiguration(
        check = "overflow clear",
        stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, growEnable = true, dataWidth = 4, binPoint = 1),
        inputPattern = safeCornerPattern,
      ),
    ).iterator
  } yield config

  // Checks SRAM-backed delay with random data by forcing the stage delay buffer into single- and dual-port memory paths.
  private val sramConfigs: Iterator[SDFStageTestConfiguration] = for {
    decimation     <- decimationSeq.iterator
    singlePortMem  <- singlePortMemSeq.iterator
  } yield SDFStageTestConfiguration(
    check = "SRAM-backed delay",
    stage = SDFStageConfiguration(stageSize = 16, decimation = decimation, bufferAsMem = true, singlePortMem = singlePortMem),
    inputPattern = randomPattern,
  )

  // Checks that all stage-local counters, control pipes, output pipes, and delay storage restart cleanly.
  private val resetConfigs: Iterator[SDFStageTestConfiguration] = for {
    decimation <- decimationSeq.iterator
    config <- Seq(
      SDFStageTestConfiguration(
        check = "stage reset between chirps",
        stage = SDFStageConfiguration(stageSize = 8, decimation = decimation, growEnable = true),
        inputPattern = safeCornerPattern,
        resetOnly = true,
      ),
      SDFStageTestConfiguration(
        check = "SRAM-backed stage reset between chirps",
        stage = SDFStageConfiguration(stageSize = 16, decimation = decimation, growEnable = true, bufferAsMem = true, singlePortMem = false),
        inputPattern = safeCornerPattern,
        resetOnly = true,
      ),
      SDFStageTestConfiguration(
        check = "single-port SRAM-backed stage reset between chirps",
        stage = SDFStageConfiguration(stageSize = 16, decimation = decimation, growEnable = true, bufferAsMem = true, singlePortMem = true),
        inputPattern = safeCornerPattern,
        resetOnly = true,
      ),
    ).iterator
  } yield config

  private val configs: Iterator[SDFStageTestConfiguration] =
    baselineConfigs ++ pipelineConfigs ++ seededConfigs ++ trimConfigs ++ fixedPointConfigs ++ controlConfigs ++ sramConfigs ++ resetConfigs

  // Registers every stage matrix row as an independent ScalaTest case with shared DUT/model checking.
  configs.foreach { config =>
    register(config)
  }
}
