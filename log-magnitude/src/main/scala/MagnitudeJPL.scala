package opera.logmagnitude

import breeze.linalg.max
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}
import dsptools._
import dsptools.numbers._
import fixedpoint.{FixedPoint, fromIntToBinaryPoint}

/**
 * Jet Propulsion Laboratory magnitude approximation.
 *
 * The approximation is defined as:
 *    A = X + Y/8 , for X >= 3Y
 *    or
 *    A = 7/8*X + 1/2*Y, for 3Y > X
 *
 * where:
 * - X = max(|I|, |Q|)
 * - Y = min(|I|, |Q|)
 *
 * Reference:
 * `Paper <https://ipnpr.jpl.nasa.gov/progress_report/42-40/40L.PDF>`_
 */
class MagnitudeJPL[T <: Data: Real: BinaryRepresentation](val params: LogMagnitudeParams[T]) extends Module {
  val addPipeRegs: Int = if (params.addPipeRegs) 1 else 0

  // Data widths
  val inputWidth: Int = params.inputType.getWidth / 2
  val outputWidth: Int = params.outputType.getWidth
  // Data binary points
  val inputBinPoint = params.inputType.real match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }
  val outputBinPoint = params.outputType match {
    case data: FixedPoint => data.binaryPoint.get
    case _ => 0
  }
  // Requirement for correct result
  require((outputWidth - outputBinPoint) >= (inputWidth - inputBinPoint + 2))

  // IO
  val io: LogMagnitudeIO[T] = IO(new LogMagnitudeIO(params))

  // Get I (real) and Q (imaginary) absolute values
  val absI: UInt = Real[T].abs(io.in.bits.real).asUInt
  val absQ: UInt = Real[T].abs(io.in.bits.imag).asUInt
  // Calculate X and Y
  val x: UInt = Wire(UInt(io.in.bits.real.getWidth.W))
  val y: UInt = Wire(UInt(io.in.bits.real.getWidth.W))
  when (absI > absQ) {
    x := absI
    y := absQ
  }.otherwise {
    x := absQ
    y := absI
  }

  // Check condition X >= 3Y
  private val y3: UInt = y +& (y << 1).asUInt
  private val x_ge_y3 = Wire(Bool())
  private val r_x_ge_y3: Option[Vec[Bool]] = if (params.addPipeRegs) Some(Reg(Vec(2 * addPipeRegs, x_ge_y3.cloneType))) else None
  when(x >= y3) {
    x_ge_y3 := true.B
  }.otherwise {
    x_ge_y3 := false.B
  }

  // geA = 1.0 * X + 1/8 * Y;  X >= 3Y
  // Align geA (greater or equal A) with leA (less or equal A)
  val geA: UInt = x +& (y >> 3).asUInt
  private val r_geA: Option[Vec[UInt]] = if (params.addPipeRegs) Some(Reg(Vec(2 * addPipeRegs, geA.cloneType))) else None

  // We want to avoid multiplication 7/8 * X. So we will instead subtract 1/8*X from X
  private val x_7_8: UInt = x -& (x >> 3).asUInt
  private val r_x_7_8: Option[Vec[UInt]] = if (params.addPipeRegs) Some(Reg(Vec(addPipeRegs, x_7_8.cloneType))) else None
  private val r_y_half: Option[Vec[UInt]] = if (params.addPipeRegs) Some(Reg(Vec(addPipeRegs, (y >> 1).asUInt.cloneType))) else None

  // leA = 7/8 * X + 1/2 * Y;  X <= 3Y
  val leA: UInt =
    if (params.addPipeRegs)
      r_x_7_8.get.head +& r_y_half.get.head
    else
      x_7_8 +& (y >> 1).asUInt
  private val r_leA: Option[Vec[UInt]] = if (params.addPipeRegs) Some(Reg(Vec(addPipeRegs, leA.cloneType))) else None

  private val A = Wire(UInt(max(geA.getWidth,leA.getWidth).W))
    if (params.addPipeRegs) {
      when(r_x_ge_y3.get.last) {
        A := r_geA.get.last.zext.asUInt
      }. otherwise {
        A := r_leA.get.last.zext.asUInt
      }
    } else {
      when(x_ge_y3) {
        A := geA.zext.asUInt
      }.otherwise {
        A := leA.zext.asUInt
      }
    }

  if (inputBinPoint > outputBinPoint) {
    io.out.bits := DspContext.alter(DspContext.current.copy(binaryPointGrowth = 0, trimType = params.trimType)) {
      val dataWidth = inputBinPoint + (params.outputType.getWidth - outputBinPoint)
      A.zext.asTypeOf(FixedPoint(dataWidth.W, inputBinPoint.BP)).div2(inputBinPoint - outputBinPoint)
    }.asTypeOf(io.out.bits)
  } else
    io.out.bits := A.zext.asTypeOf(io.out.bits)

  // Handshake control
  if (params.addPipeRegs) {
    val r_last    = Reg(Vec(2 * addPipeRegs, Bool()))
    val handshake = AlignHandshake(2 * addPipeRegs, io.in.valid, io.out.ready)

    for (i <- 0 until 2 * addPipeRegs) {
      when(handshake._1(i)) {
        if (i == 0) {
          r_last(i)          := io.i_last
          r_geA.get(i)       := geA
          r_x_7_8.get.head   := x_7_8
          r_y_half.get.head  := (y >> 1).asUInt
          r_x_ge_y3.get.head := x_ge_y3
        }
        else {
          r_last(i)        := r_last(i-1)
          r_geA.get(i)     := r_geA.get(i-1)
          r_leA.get.head   := leA
          r_x_ge_y3.get(i) := r_x_ge_y3.get(i-1)
        }
      }
    }
    io.o_last    := r_last.last
    io.in.ready  := handshake._1.head
    io.out.valid := handshake._2.last
  }
  else {
    io.out.valid := io.in.valid
    io.in.ready  := io.out.ready
    io.o_last    := io.i_last
  }
}

object MagnitudeJPLApp extends App {
  val params = LogMagnitudeParams[FixedPoint](
    inputType    = DspComplex(FixedPoint(16.W, 14.BP)),
    outputType   = FixedPoint(20.W, 14.BP),
    magType      = JPL,
    addPipeRegs  = false,
    trimType     = Convergent
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new MagnitudeJPL(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/MagnitudeJPL"))
  )
}
