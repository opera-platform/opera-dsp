package preprocessing

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util.{circt => _, _}
import circt.stage.{ChiselStage, FirtoolOption}

case class CRCParameters(
  dataBytes: Int,
  polynomial: Long,
  init: Long,
  reflectIn: Boolean,
  reflectOut: Boolean,
  xorOut: Long
)

class CRCIO(dataBytes: Int) extends Bundle {
  val i_data: UInt = Input(UInt((dataBytes*8).W))
  val i_en: Bool = Input(Bool())
  val i_done: Bool = Input(Bool())
  val o_crc: UInt = Output(UInt(32.W))
}

class CRC(val params: CRCParameters) extends Module {
  // IOs
  val io: CRCIO = IO(new CRCIO(params.dataBytes))

  val crcReg: UInt = RegInit(params.init.U(32.W))

  def byteCRC(currentCRC: UInt, byte: UInt): UInt = {
    val inputByte = if (params.reflectIn) Reverse(byte) else byte
    var crc: UInt = currentCRC ^ (inputByte << 24).asUInt
    for (_ <- 0 until 8) {
      crc = Mux(crc(31), (crc << 1) ^ params.polynomial.U, crc << 1)
    }
    crc
  }

  when(io.i_en) {
    val byteVec = Wire(Vec(params.dataBytes, UInt(8.W)))
    byteVec := io.i_data.asTypeOf(Vec(params.dataBytes, UInt(8.W)))
    var nextCRC = crcReg

    for (i <- 0 until params.dataBytes) {
      nextCRC = byteCRC(nextCRC, byteVec(i))
    }
    crcReg := nextCRC
  }.elsewhen(io.i_done) {
    crcReg := params.init.U
  }

  val finalCrc = if (params.reflectOut) Reverse(crcReg) else crcReg
  io.o_crc := finalCrc ^ params.xorOut.U
}

object CRCApp extends App {
  val params = CRCParameters(
    dataBytes = 2,
    polynomial = 0x04C11DB7,
    init = 0xFFFFFFFFL,
    reflectIn = false,
    reflectOut = false,
    xorOut = 0x00000000L
  )

  (new ChiselStage).execute(
    Array("--target", "systemverilog"),
    Seq(ChiselGeneratorAnnotation(() => new CRC(params)),
      FirtoolOption("--disable-all-randomization"),
      FirtoolOption("--split-verilog"),
      FirtoolOption("--o=./rtl/CRC"))
  )
}


