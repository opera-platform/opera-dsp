package opera.fft

import chisel3._
import chisel3.fromIntToWidth
import dsptools._
import dsptools.numbers._
import fixedpoint._
import freechips.rocketchip.diplomacy.AddressSet
import play.api.libs.json.Json

object Utils {
  /** Pipelined complex multiply with explicit DSP context settings and trim point from the input type. */
  def complexMul(
      input       : DspComplex[FixedPoint],
      twiddle     : DspComplex[FixedPoint],
      inputType   : DspComplex[FixedPoint],
      numAddPipes : Int,
      numMulPipes : Int,
      trimType    : TrimType,
      use4Muls    : Boolean,
  ): DspComplex[FixedPoint] = {
    val bpos = inputType.real.binaryPoint.get
    DspContext.alter(DspContext.current.copy(
      numAddPipes     = numAddPipes,
      numMulPipes     = numMulPipes,
      trimType        = trimType,
      overflowType    = Grow,
      complexUse4Muls = use4Muls
    )) { input.context_*(twiddle).trimBinary(bpos) }
  }

  /** Resize complex data through normal Chisel assignment semantics. */
  def resizeComplex(data: DspComplex[FixedPoint], proto: DspComplex[FixedPoint]): DspComplex[FixedPoint] = {
    val w_out = Wire(proto)
    w_out := data
    w_out
  }

  /** True when any FixedPoint butterfly lane overflowed by one sign bit. */
  def butterflyOverflow(butterfly: Seq[DspComplex[FixedPoint]]): Bool =
    butterfly.flatMap(data => Seq(data.real, data.imag)).map { data =>
      val u = data.asUInt
      u(data.getWidth - 1) ^ u(data.getWidth - 2)
    }.foldLeft(false.B)(_ || _)

  /** Select pass-through or rounded divide-by-2 butterfly outputs. */
  def scaleButterfly(
      butterfly  : Seq[DspComplex[FixedPoint]],
      outType    : DspComplex[FixedPoint],
      divBy2     : Bool,
      growEnable : Boolean,
      trimType   : TrimType,
  ): (Seq[DspComplex[FixedPoint]], Bool) = {
    require(butterfly.length == 2, s"scaleButterfly expects two lanes, got ${butterfly.length}")
    val w_scaled = Seq.fill(2)(Wire(outType))

    if (growEnable) {
      w_scaled.zip(butterfly).foreach { case (w_out, data) => w_out := data }
      (w_scaled, false.B)
    } else {
      val divided = butterfly.map { data =>
        DspContext.alter(DspContext.current.copy(trimType = trimType, binaryPointGrowth = 0)) {
          data.div2(1)
        }
      }
      w_scaled.zip(butterfly.zip(divided)).foreach {
        case (w_out, (pass, half)) => w_out := Mux(divBy2, resizeComplex(half, outType), resizeComplex(pass, outType))
      }
      (w_scaled, butterflyOverflow(butterfly))
    }
  }

  /** Conditionally rotate complex data by -j (swap real/imag and negate new imag). */
  def invertComplexData(data: DspComplex[FixedPoint], invertSig: Bool): DspComplex[FixedPoint] = {
    val w_out = Wire(data.cloneType)
    w_out.real := Mux(invertSig,  data.imag, data.real)
    w_out.imag := Mux(invertSig, -data.real, data.imag)
    w_out
  }

  /** Drive dst from src, swapping real/imag when the direction selects IFFT ordering. */
  def assignFftOutputByDirection(src: DspComplex[FixedPoint], dst: DspComplex[FixedPoint], fftOrIfft: Bool): Unit =
    when(fftOrIfft) { dst := src }
    .otherwise      { dst.real := src.imag; dst.imag := src.real }
}

object ParseParameters {
  def parseconfig(filename: String): Either[(AddressSet, FFTParams, Int), Throwable] = {
    try {
      val resource = scala.io.Source.fromFile(filename)
      val content = Json.parse(resource.getLines().mkString)

      val address = AddressSet(
        base = BigInt((content \ "address" \ "base").get.as[String].stripSuffix("L").drop(2), 16),
        mask = BigInt((content \ "address" \ "mask").get.as[String].stripSuffix("L").drop(2), 16)
      )

      val parameters = content \ "parameters"
      val fftParams = FFTParams(
        fftSize = (parameters \ "fftSize").get.as[Int],
        twiddleType = DspComplex(
          FixedPoint(
            (parameters \ "twiddleWidth").get.as[Int].W,
            (parameters \ "twiddleBinPoint").get.as[Int].BP
          )
        ),
        inDataType = DspComplex(
          FixedPoint(
            (parameters \ "inputWidth").get.as[Int].W,
            (parameters \ "inputBinPoint").get.as[Int].BP
          )
        ),
        decimation       = parseDecimation((parameters \ "decimation").get.as[String]),
        sdfRadix         = parseSdfRadix((parameters \ "sdfRadix").get.as[String]),
        growEnable       = (parameters \ "growEnable").get.as[Seq[Boolean]],
        runTime          = (parameters \ "runTime").get.as[Boolean],
        divBy2           = (parameters \ "divBy2").get.as[Seq[Boolean]],
        divBy2Reg        = (parameters \ "divBy2Reg").get.as[Boolean],
        overflowReg      = (parameters \ "overflowReg").get.as[Boolean],
        trimType         = parseTrimType((parameters \ "trimType").get.as[String]),
        numAddPipes      = (parameters \ "numAddPipes").get.as[Int],
        numMulPipes      = (parameters \ "numMulPipes").get.as[Int],
        direction        = (parameters \ "direction").get.as[Boolean],
        directionReg     = (parameters \ "directionReg").get.as[Boolean],
        use4Muls         = (parameters \ "use4Muls").get.as[Boolean],
        useBitReverse    = (parameters \ "useBitReverse").get.as[Boolean],
        minSRAMdepth     = (parameters \ "minSRAMdepth").get.as[Int],
        singlePortSRAM   = (parameters \ "singlePortSRAM").get.as[Boolean],
        stageTrimTypes   = (parameters \ "stageTrimTypes").get.as[Seq[String]].map(parseTrimType),
        twiddleTrimTypes = (parameters \ "twiddleTrimTypes").get.as[Seq[String]].map(parseTrimType)
      )
      val beatBytes = (parameters \ "beatBytes").get.as[Int]

      Left((address, fftParams, beatBytes))
    } catch {
      case e: Throwable => Right(e)
    }
  }

  private def parseDecimation(value: String): DecimationType =
    value match {
      case "DIF" => DIF
      case "DIT" => DIT
      case other => throw new IllegalArgumentException(s"Unsupported FFT decimation type: $other")
    }

  private def parseSdfRadix(value: String): SDFRadix =
    value match {
      case "Radix2"  => Radix2
      case "Radix22" => Radix22
      case other     => throw new IllegalArgumentException(s"Unsupported FFT SDF radix: $other")
    }

  private def parseTrimType(value: String): TrimType =
    value match {
      case "Floor"                    => Floor
      case "Ceiling"                  => Ceiling
      case "Convergent"               => Convergent
      case "Round"                    => Round
      case "RoundDown"                => RoundDown
      case "RoundUp"                  => RoundUp
      case "RoundTowardsZero"         => RoundTowardsZero
      case "RoundTowardsInfinity"     => RoundTowardsInfinity
      case "RoundHalfDown"            => RoundHalfDown
      case "RoundHalfUp"              => RoundHalfUp
      case "RoundHalfTowardsZero"     => RoundHalfTowardsZero
      case "RoundHalfTowardsInfinity" => RoundHalfTowardsInfinity
      case "RoundHalfToEven"          => RoundHalfToEven
      case "RoundHalfToOdd"           => RoundHalfToOdd
      case other                      => throw new IllegalArgumentException(s"Unsupported FFT trim type: $other")
    }
}
