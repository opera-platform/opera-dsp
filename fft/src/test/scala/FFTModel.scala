package opera.fft

import chisel3.util.log2Ceil
import dsptools._
import dsptools.numbers._
import fixedpoint.FixedPoint
import ModelUtils.{FixedFormat, Pipe, RawComplex}

/**
 * Pure Scala bit-accurate FixedPoint model for SDF FFT configurations.
 *
 * The model reuses the R2SDF and R22SDF stage
 * models, returns samples in DUT output order.
 */
object FFTModel {
  /**
   * Output samples produced by the SDF FFT model.
   *
   * @param samples         Raw output stream samples in DUT order.
   * @param overflowByCycle Per-modeled-cycle overflow flags in DUT stage order.
   */
  final case class ModelResult(samples: Vector[RawComplex], overflowByCycle: Vector[Vector[Boolean]] = Vector.empty) {
    /**
     * Returns the first modeled FFT output frame.
     *
     * @param size   Number of output samples in the checked frame.
     */
    def checkedFrame(size: Int): Vector[RawComplex] = samples.take(size)

    /**
     * Returns overflow flags for one stage across all modeled cycles.
     *
     * @param stage Stage index in DUT overflow-vector order.
     */
    def stageOverflow(stage: Int): Vector[Boolean] =
      overflowByCycle.map { cycle =>
        require(stage >= 0 && stage < cycle.length, s"stage $stage is outside overflow vector width ${cycle.length}")
        cycle(stage)
      }

    /**
     * Indicates whether any modeled cycle overflowed in any stage.
     */
    def anyOverflow: Boolean = overflowByCycle.exists(_.exists(identity))
  }

  private final case class RunTrace(samples: Vector[RawComplex], overflowByCycle: Vector[Vector[Boolean]])

  /**
   * Runs the model selected by `params.sdfRadix`.
   *
   * @param params FixedPoint FFT parameters.
   * @param input  Raw complex input samples in DUT input order.
   */
  def apply(params: FFTParams, input: Seq[RawComplex]): ModelResult =
    params.sdfRadix match {
      case Radix2  => r2 (params, input)
      case Radix22 => r22(params, input)
    }

  /**
   * Runs the radix-2 SDF FFT model.
   *
   * @param params FixedPoint FFT parameters with `sdfRadix = Radix2`.
   * @param input  Raw complex input samples in DUT input order.
   */
  def r2(params: FFTParams, input: Seq[RawComplex]): ModelResult = {
    requireFullSize(params)
    val model = new R2(params)
    val trace = model.run(input.toVector)
    ModelResult(trace.samples, trace.overflowByCycle)
  }

  /**
   * Runs the radix-2^2 SDF FFT model.
   *
   * @param params FixedPoint FFT parameters with `sdfRadix = Radix22`.
   * @param input  Raw complex input samples in DUT input order.
   */
  def r22(params: FFTParams, input: Seq[RawComplex]): ModelResult = {
    requireFullSize(params)
    require(
      params.fftSize >= 4 && (log2Ceil(params.fftSize) & 1) == 0,
      s"FFTModel R22 supports only 4^N FFT sizes, got fftSize=${params.fftSize}"
    )
    val model = new R22(params)
    val trace = model.run(input.toVector)
    ModelResult(trace.samples, trace.overflowByCycle)
  }

  /**
   * FixedPoint format of the FFT input type.
   *
   * @param params FFT parameters to inspect.
   */
  def inputFormat(params: FFTParams): FixedFormat = formatOf(params.inDataType)

  /** FixedPoint input format used by one internal FFT stage. */
  def stageInputFormat(params: FFTParams, stage: Int): FixedFormat = formatOf(params.stageInputType(stage))

  /** FixedPoint output format used by one internal FFT stage. */
  def stageOutputFormat(params: FFTParams, stage: Int): FixedFormat = formatOf(params.stageOutputType(stage))

  /** FixedPoint format of the FFT output type. */
  def fftOutputFormat(params: FFTParams): FixedFormat = formatOf(params.fftOutputType)

  def stageFormat(params: FFTParams, stage: Int): FixedFormat = stageOutputFormat(params, stage)

  /**
   * FixedPoint format of the twiddle coefficients.
   *
   * @param params FFT parameters to inspect.
   */
  def twiddleFormat(params: FFTParams): FixedFormat = formatOf(params.twiddleType)

  /**
   * Extracts a known FixedPoint format from a DspComplex type.
   *
   * @param dataType Complex FixedPoint type to inspect.
   */
  def formatOf(dataType: DspComplex[FixedPoint]): FixedFormat = ModelUtils.formatOf(dataType)

  private[fft] def radix2Twiddle(address: Int, stageN: Int, fftSize: Int, format: FixedFormat): RawComplex =
    quarterWaveTwiddle(address, stageN, fftSize, format)

  private[fft] def radix22Twiddle(address: Int, stageN: Int, fftSize: Int, format: FixedFormat): RawComplex = {
    require(stageN >= 4, "radix-2^2 twiddle stage must be at least 4")
    val idWidth         = log2Ceil(stageN)
    val nDiv4           = stageN / 4
    val lowWidth        = log2Ceil(nDiv4)
    val addressMask     = (1 << idWidth) - 1
    val addressInStage  = address & addressMask
    val addressQuadrant = addressInStage >> (idWidth - 2)
    val lowMask         = if (lowWidth == 0) 0 else (1 << lowWidth) - 1
    val a               = if (lowWidth == 0) 0 else addressInStage & lowMask
    val k = addressQuadrant match {
      case 0 => 0
      case 1 => a << 1
      case 2 => a
      case 3 => a + (a << 1)
    }
    quarterWaveTwiddle(k, stageN, fftSize, format)
  }

  private def requireFullSize(params: FFTParams): Unit = {
    require(!params.runTime,      "FFTModel currently models full-size FFTs only")
    require(!params.divBy2Reg,    "FFTModel currently requires divBy2 settings")
    require(!params.directionReg, "FFTModel currently requires FFT direction")
  }

  private def quarterWaveTwiddle(address: Int, stageN: Int, fftSize: Int, format: FixedFormat): RawComplex = {
    require(stageN >= 4,     "twiddle stage must be at least 4")
    require(stageN % 4 == 0, "twiddle stage must be divisible by 4")
    require(fftSize % stageN == 0, s"FFT size $fftSize must be a multiple of stageN $stageN")
    val k = address & (stageN - 1)
    val theta = -2.0 * math.Pi * k.toDouble / stageN.toDouble
    RawComplex(
      format.wrap(ModelUtils.roundToFittingRaw(format, math.cos(theta))),
      format.wrap(ModelUtils.roundToFittingRaw(format, math.sin(theta)))
    )
  }

  private def invert(sample: RawComplex, format: FixedFormat): RawComplex =
    RawComplex(format.wrap(sample.imag), format.wrap(-sample.real))

  private def invertWhen(sample: RawComplex, enable: Boolean, format: FixedFormat): RawComplex =
    if (enable) invert(sample, format) else sample.map(format.wrap)

  private def finalOutput(params: FFTParams, sample: RawComplex): RawComplex = {
    val assigned = if (params.direction) sample else RawComplex(sample.imag, sample.real)
    assigned.map(fftOutputFormat(params).wrap)
  }

  private def complexMulLatency(params: FFTParams): Int =
    if (params.use4Muls) params.numAddPipes + params.numMulPipes else 2 * params.numAddPipes + params.numMulPipes

  private def runCycles(params: FFTParams, inputLength: Int): Int =
    inputLength + (params.numAddPipes + complexMulLatency(params)) * log2Ceil(params.fftSize) + params.fftSize + 64

  private def lowCounter(counter: Int, delay: Int): Int =
    if (delay == 1) 0 else counter & (delay - 1)

  private final class ComplexPipe(format: FixedFormat, twFormat: FixedFormat, latency: Int, trimType: TrimType, use4Muls: Boolean) {
    private val zero = RawComplex(0, 0)
    private val pipe = new Pipe[RawComplex](latency, zero)

    def step(input: RawComplex, twiddle: RawComplex): RawComplex =
      pipe.step(ModelUtils.complexMul(input, twiddle, format, twFormat, trimType, use4Muls))
  }

  private final class TwiddleOrBypass(format: FixedFormat, twFormat: FixedFormat, latency: Int, trimType: TrimType, use4Muls: Boolean) {
    private val zero     = RawComplex(0, 0)
    private val passPipe = new Pipe[RawComplex](latency, zero)
    private val mulPipe  = new Pipe[RawComplex](latency, zero)
    private val enPipe   = new Pipe[Boolean](latency, false)

    def step(data: RawComplex, twiddle: RawComplex, twiddleEn: Boolean): RawComplex = {
      val pass = passPipe.step(data.map(format.wrap))
      val mul  = mulPipe.step(ModelUtils.complexMul(data, twiddle, format, twFormat, trimType, use4Muls))
      val en   = enPipe.step(twiddleEn)
      if (en) mul else pass
    }
  }

  private final class R2(params: FFTParams) {
    private val isDIF           = params.decimation == DIF
    private val noOfStages      = log2Ceil(params.fftSize)
    private val fullSize        = 1 << noOfStages
    private val stageDelays     = (if (isDIF) (0 until noOfStages).reverse else 0 until noOfStages).map(1 << _).toVector
    private val stageSizes      = stageDelays.map(_ << 1)
    private val latency         = complexMulLatency(params)
    private val twFormat        = twiddleFormat(params)
    private val zero            = RawComplex(0, 0)
    private val stageHasTwiddle = (0 until noOfStages).map(i => if (isDIF) i != noOfStages - 1 else i != 0).toVector
    private val stages = stageDelays.zipWithIndex.map { case (delay, i) =>
      new FFTStageModel(radixParams(i, stageSizes(i), delay))
    }
    private val links = (0 until noOfStages).map { i =>
      val format = if (isDIF) stageOutputFormat(params, i) else stageInputFormat(params, i)
      new TwiddleOrBypass(format, twFormat, latency, params.resolvedTwiddleTrimTypes(i), params.use4Muls)
    }.toVector
    private val difTwiddlePipes = (0 until noOfStages).map(_ => new Pipe[RawComplex](params.numAddPipes, zero)).toVector
    private val difTwiddleEnPipes = (0 until noOfStages).map(_ => new Pipe[Boolean](params.numAddPipes, false)).toVector

    def run(input: Vector[RawComplex]): RunTrace = {
      val out = Vector.newBuilder[RawComplex]
      val overflowByCycle = Vector.newBuilder[Vector[Boolean]]
      val totalCycles = runCycles(params, input.length)
      var outputCount = 0

      for (cycle <- 0 until totalCycles) {
        val inputFire     = cycle < input.length
        // Continue with zero samples after the provided stream ends so finite test vectors drain fully.
        val coreFire      = inputFire || outputCount < input.length
        var data          = if (inputFire) input(cycle) else zero
        var en            = coreFire
        var lastData      = zero
        var lastValid     = false
        val cycleOverflow = Array.fill(noOfStages)(false)

        for (i <- 0 until noOfStages) {
          val stage   = stages(i)
          val delay   = stageDelays(i)
          val counter = stage.currentCounter
          val low     = lowCounter(counter, delay)
          val twiddle = stageTwiddle(i, low)

          if (isDIF) {
            val result         = stage.step(data, en, params.stageDivBy2(i))
            cycleOverflow(i)   = result.overflow
            val rawEn          = stageHasTwiddle(i) && ((counter & delay) == 0) && low != 0
            val alignedTwiddle = difTwiddlePipes(i).step(twiddle)
            val alignedEn      = difTwiddleEnPipes(i).step(rawEn)
            data = links(i).step(result.output, alignedTwiddle, alignedEn)
            en   = result.valid
          } else {
            val rawEn        = stageHasTwiddle(i) && counter > delay
            val inputToStage = links(i).step(data, twiddle, rawEn)
            val result       = stage.step(inputToStage, en, params.stageDivBy2(i))
            cycleOverflow(i) = result.overflow
            data = result.output
            en   = result.valid
          }

          if (i == noOfStages - 1) {
            lastData  = data
            lastValid = en
          }
        }

        overflowByCycle += cycleOverflow.toVector
        if (lastValid && outputCount < input.length) {
          out += finalOutput(params, lastData)
          outputCount += 1
        }
      }

      require(outputCount == input.length, s"FFTModel R2 produced $outputCount valid samples for ${input.length} inputs")
      RunTrace(out.result(), overflowByCycle.result())
    }

    private def stageTwiddle(i: Int, counter: Int): RawComplex =
      if (stageSizes(i) >= 4) radix2Twiddle(counter, stageSizes(i), fullSize, twFormat) else zero

    private def radixParams(index: Int, stageSize: Int, delay: Int): RadixParams =
      RadixParams(
        inDataType    = params.stageInputType(index),
        outDataType   = params.stageOutputType(index),
        twiddleType   = params.twiddleType,
        stageSize     = stageSize,
        decimation    = params.decimation,
        overflowReg   = params.overflowReg,
        divBy2Reg     = false,
        divBy2        = params.stageDivBy2(index),
        growEnable    = params.stageGrowEnable(index),
        latency       = latency,
        addPipeRegs   = params.numAddPipes,
        mulPipeRegs   = params.numMulPipes,
        dspMul4       = params.use4Muls,
        delay         = delay,
        bufferAsMem   = false,
        singlePortMem = false,
        trimType      = params.resolvedStageTrimTypes(index),
      )
  }

  private final class R22(params: FFTParams) {
    private val isDIF                  = params.decimation == DIF
    private val noOfStages             = log2Ceil(params.fftSize)
    private val fullSize               = 1 << noOfStages
    private val evenNoOfStages         = noOfStages % 2 == 0
    private val noOfTwiddles           = (noOfStages - 1) / 2
    private val stageDelays            = (if (isDIF) (0 until noOfStages).reverse else 0 until noOfStages).map(1 << _).toVector
    private val cumulativeDelays       = stageDelays.scanLeft(0)(_ + _).toVector
    private val latency                = complexMulLatency(params)
    private val stageLatency           = params.numAddPipes + latency
    private val twFormat               = twiddleFormat(params)
    private val zero                   = RawComplex(0, 0)
    private val stageRoleIndices       = (0 until noOfStages).map(i => if (isDIF) i else noOfStages - i - 1).toVector
    private val stageOdd               = stageRoleIndices.map(i => (i & 1) == 1)
    private val stageHasTwiddleControl = (0 until noOfStages).map(i => if (isDIF) i != noOfStages - 1 else i != 0).toVector
    private val stages = stageDelays.zipWithIndex.map { case (delay, i) =>
      val stageSize   = if (isDIF) params.fftSize >> i else 2 << i
      val counterInit = if (isDIF) None else Some(((stageSize / 2) + 1) & (stageSize - 1))
      new FFTStageModel(radixParams(i, stageSize, delay), counterInitOverride = counterInit)
    }
    private val twiddlePipes    = (0 until noOfStages).map(_ => new Pipe[RawComplex](params.numAddPipes, zero)).toVector
    private val invertPipes     = (0 until noOfStages).map(_ => new Pipe[Boolean](params.numAddPipes, false)).toVector
    private val passPipes       = (0 until noOfStages).map(_ => new Pipe[RawComplex](latency, zero)).toVector
    private val invertDataPipes = (0 until noOfStages).map(_ => new Pipe[RawComplex](latency, zero)).toVector
    private val enablePipes     = (0 until noOfStages).map(_ => new Pipe[Boolean](stageLatency, false)).toVector
    private val counterPipes    = (0 until noOfStages).map(_ => new Pipe[Int](stageLatency, 0)).toVector
    private val outputValidPipe = new Pipe[Boolean](stageLatency, false)
    private val mulPipes = (0 until noOfStages).map { i =>
      val format = if (isDIF) stageOutputFormat(params, i) else stageInputFormat(params, i)
      new ComplexPipe(format, twFormat, latency, params.resolvedTwiddleTrimTypes(i), params.use4Muls)
    }.toVector
    private val tailPipe = new Pipe[RawComplex](latency, zero)

    def run(input: Vector[RawComplex]): RunTrace = {
      val out                  = Vector.newBuilder[RawComplex]
      val overflowByCycle      = Vector.newBuilder[Vector[Boolean]]
      val totalCycles          = runCycles(params, input.length)
      var outputCount          = 0
      var coreCounter          = 0
      var initialOutDone       = false

      for (cycle <- 0 until totalCycles) {
        val inputFire         = cycle < input.length
        // Continue with zero samples after the provided stream ends so finite test vectors drain fully.
        val coreFire          = inputFire || outputCount < input.length
        val scheduledEnables  = Array.fill(noOfStages)(false)
        val scheduledCounters = Array.fill(noOfStages)(0)
        var scheduledEnable   = coreFire
        var scheduledCounter  = coreCounter
        for (i <- 0 until noOfStages) {
          scheduledEnables(i)  = scheduledEnable
          scheduledCounters(i) = scheduledCounter
          scheduledEnable      = enablePipes(i).step(scheduledEnable)
          scheduledCounter     = counterPipes(i).step(scheduledCounter)
        }

        val control       = r22Control(scheduledCounters)
        var data          = if (inputFire) input(cycle) else zero
        val stageOutputs  = Array.fill(noOfStages)(zero)
        val mulOutputs    = Array.fill(noOfStages)(zero)
        val cycleOverflow = Array.fill(noOfStages)(false)

        for (i <- 0 until noOfStages) {
          val en = scheduledEnables(i)
          if (isDIF) {
            val result       = stages(i).step(data, en, params.stageDivBy2(i))
            cycleOverflow(i) = result.overflow
            stageOutputs(i)  = result.output
            val inverted = invertWhen(result.output, control.invertSignals(i), stageOutputFormat(params, i))
            val next = if (i == 0 || i == noOfStages - 1) {
              val passData = if (i == 0) inverted else result.output
              passPipes(i).step(passData)
            } else if ((i & 1) == 1) {
              mulOutputs(i) = mulPipes(i).step(result.output, control.twiddles(i))
              mulOutputs(i)
            } else {
              mulOutputs(i) = mulOutputs(i - 1)
              invertDataPipes(i).step(inverted)
            }
            data = next
          } else {
            val prevOut        = data
            val inputFormat    = stageInputFormat(params, i)
            val prevStageInput = prevOut.map(inputFormat.wrap)
            val inverted       = invertWhen(prevStageInput, control.invertSignals(i), inputFormat)
            val stageIn = if (i == noOfStages - 1 || i == 0) {
              val passData = if (i == noOfStages - 1) inverted else prevStageInput
              passPipes(i).step(passData)
            } else if ((evenNoOfStages && i % 2 == 0) || (!evenNoOfStages && i % 2 == 1)) {
              val fbData = if (evenNoOfStages) stageOutputs(i - 2).map(inputFormat.wrap) else stageOutputs(i).map(inputFormat.wrap)
              val fbTw   = if (evenNoOfStages) control.twiddles(i - 1) else control.twiddles(i + 1)
              val multIn = if (stageOdd(i)) prevStageInput else fbData
              val multTw = if (stageOdd(i)) control.twiddles(i) else fbTw
              mulOutputs(i) = mulPipes(i).step(multIn, multTw)
              if (stageOdd(i)) mulOutputs(i) else invertDataPipes(i).step(inverted)
            } else {
              mulOutputs(i) = if (evenNoOfStages) {
                if (i + 1 < noOfStages) mulOutputs(i + 1) else zero
              } else {
                if (i > 0) mulOutputs(i - 1) else zero
              }
              if (stageOdd(i)) mulOutputs(i) else invertDataPipes(i).step(inverted)
            }
            val result = stages(i).step(stageIn, en, params.stageDivBy2(i))
            cycleOverflow(i) = result.overflow
            stageOutputs(i)  = result.output
            data = result.output
          }
        }

        if (scheduledEnables.last && scheduledCounters.last == fullSize - 1) {
          initialOutDone = true
        }
        val r22OutputValid = outputValidPipe.step(scheduledEnables.last && initialOutDone)
        val stageTail = if (isDIF) tailPipe.step(stageOutputs.last) else stageOutputs.last
        overflowByCycle += cycleOverflow.toVector
        if (r22OutputValid && outputCount < input.length) {
          out += finalOutput(params, stageTail)
          outputCount += 1
        }

        if (coreFire) {
          coreCounter = if (coreCounter == fullSize - 1) 0 else coreCounter + 1
        }
      }

      require(outputCount == input.length, s"FFTModel R22 produced $outputCount valid samples for ${input.length} inputs")
      RunTrace(out.result(), overflowByCycle.result())
    }

    private final class Control(
      val twiddles: Vector[RawComplex],
      val invertSignals: Vector[Boolean],
    )

    private def r22Control(scheduledCounters: Array[Int]): Control = {
      val twiddleAddress = Array.fill(noOfStages)(0)
      val rawTwiddle     = Array.fill(noOfStages)(zero)
      val rawInvert      = Array.fill(noOfStages)(false)
      val twiddles       = Array.fill(noOfStages)(zero)
      val invertSignals  = Array.fill(noOfStages)(false)

      for (i <- 0 until noOfStages) {
        val counter = stages(i).currentCounter
        val delay = stageDelays(i)
        if (stageHasTwiddleControl(i)) {
          if (stageOdd(i)) {
            twiddleAddress(i) = if (isDIF) {
              (scheduledCounters(i) - cumulativeDelays(i + 1)) & (fullSize - 1)
            } else {
              (scheduledCounters(i) - cumulativeDelays(i)) & (fullSize - 1)
            }
          } else {
            rawInvert(i) =
              if (isDIF) counter >= (delay >> 1) && counter < delay
              else counter >= delay * 3 / 2
          }
        }
      }

      val lookupTwiddles = Array.fill(noOfTwiddles)(zero)
      if (noOfTwiddles > 0) {
        val evenOff       = if (evenNoOfStages) 1 else 0
        val normalOffset  = if (isDIF) 1 else evenOff + 1
        val lookupAddress = Array.fill(noOfTwiddles)(0)
        for (i <- 0 until noOfTwiddles) {
          val sourceStage = (i << 1) + normalOffset
          if (sourceStage < noOfStages) lookupAddress(i) = twiddleAddress(sourceStage)
        }
        (0 until noOfStages by 2).dropRight(1).zipWithIndex.foreach { case (m, i) =>
          val stageN = 1 << (noOfStages - m)
          val lookupIndex = if (isDIF) i else noOfTwiddles - 1 - i
          lookupTwiddles(lookupIndex) = radix22Twiddle(lookupAddress(lookupIndex), stageN, fullSize, twFormat)
        }
      }

      for (i <- 0 until noOfStages) {
        if (stageHasTwiddleControl(i)) {
          if (stageOdd(i)) {
            rawTwiddle(i) =
              if (noOfTwiddles == 0) zero
              else if (noOfTwiddles == 1) lookupTwiddles(0)
              else {
                val idx = if (isDIF || !evenNoOfStages) i >> 1 else (i - 1) >> 1
                if (idx >= 0 && idx < noOfTwiddles) lookupTwiddles(idx) else zero
              }
            twiddles(i) = if (isDIF) twiddlePipes(i).step(rawTwiddle(i)) else rawTwiddle(i)
          } else {
            invertSignals(i) = if (isDIF) invertPipes(i).step(rawInvert(i)) else rawInvert(i)
          }
        }
      }

      new Control(twiddles.toVector, invertSignals.toVector)
    }

    private def radixParams(index: Int, stageSize: Int, delay: Int): RadixParams =
      RadixParams(
        inDataType    = params.stageInputType(index),
        outDataType   = params.stageOutputType(index),
        twiddleType   = params.twiddleType,
        stageSize     = stageSize,
        decimation    = params.decimation,
        overflowReg   = params.overflowReg,
        divBy2Reg     = false,
        divBy2        = params.stageDivBy2(index),
        growEnable    = params.stageGrowEnable(index),
        latency       = latency,
        addPipeRegs   = params.numAddPipes,
        mulPipeRegs   = params.numMulPipes,
        dspMul4       = params.use4Muls,
        delay         = delay,
        bufferAsMem   = false,
        singlePortMem = false,
        trimType      = params.resolvedStageTrimTypes(index),
      )
  }
}
