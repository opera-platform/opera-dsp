package freechips.rocketchip.tilelink

import chisel3.util.log2Ceil

trait OptionalTLMasterModel extends dspblocks.MemMasterModel {
  self: chiseltest.iotesters.PeekPokeTester[_] =>
  import TLMasterModel._

  def memTL: Option[TLBundle]

  def tlReset(): Unit = {
    pokeA(AChannel())
    pokeC(CChannel())
    pokeE(EChannel())
    poke(memTL.get.a.valid, 0)
    poke(memTL.get.b.ready, 0)
    poke(memTL.get.c.valid, 0)
    poke(memTL.get.d.ready, 0)
    poke(memTL.get.e.valid, 0)
  }

  def pokeA(a: AChannel): Unit = {
    poke(memTL.get.a.bits.opcode, a.opcode)
    poke(memTL.get.a.bits.param, a.param)
    poke(memTL.get.a.bits.size, a.size)
    poke(memTL.get.a.bits.source, a.source)
    poke(memTL.get.a.bits.address, a.address)
    poke(memTL.get.a.bits.mask, a.mask)
    poke(memTL.get.a.bits.data, a.data)
  }

  def tlWriteA(a: AChannel): BigInt = {
    poke(memTL.get.a.valid, 1)
    pokeA(a)
    poke(memTL.get.d.ready, 1)
    step(1)

    while (peek(memTL.get.d.valid) == BigInt(0)) {
      step(1)
    }
    val d = peek(memTL.get.d.bits.data)
    poke(memTL.get.a.valid, 0)
    poke(memTL.get.d.ready, 0)
    d
  }

  def peekB(): BChannel = {

    val opcode = peek(memTL.get.b.bits.opcode)
    val param = peek(memTL.get.b.bits.param)
    val size = peek(memTL.get.b.bits.size)
    val source = peek(memTL.get.b.bits.source)
    val address = peek(memTL.get.b.bits.address)
    val mask = peek(memTL.get.b.bits.mask)
    val data = peek(memTL.get.b.bits.data)

    BChannel(opcode = opcode, param = param, size = size, source = source, address = address, mask = mask, data = data)
  }

  def tlReadB(): BChannel = {
    poke(memTL.get.b.ready, 1)

    while (peek(memTL.get.b.valid) != BigInt(0)) {
      step(1)
    }

    step(1)

    poke(memTL.get.b.ready, 0)

    peekB()
  }

  def pokeC(c: CChannel): Unit = {
    poke(memTL.get.c.bits.opcode, c.opcode)
    poke(memTL.get.c.bits.param, c.param)
    poke(memTL.get.c.bits.size, c.size)
    poke(memTL.get.c.bits.source, c.source)
    poke(memTL.get.c.bits.address, c.address)
    poke(memTL.get.c.bits.data, c.data)
    poke(memTL.get.c.bits.corrupt, c.corrupt)

  }

  def tlWriteC(c: CChannel): Unit = {
    poke(memTL.get.c.valid, 1)
    pokeC(c)

    while (peek(memTL.get.c.ready) != BigInt(0)) {
      step(1)
    }
    step(1)
    poke(memTL.get.c.valid, 0)
  }

  def peekD(): DChannel = {
    val opcode = peek(memTL.get.d.bits.opcode)
    val param = peek(memTL.get.d.bits.param)
    val size = peek(memTL.get.d.bits.size)
    val source = peek(memTL.get.d.bits.source)
    val sink = peek(memTL.get.d.bits.sink)
    val data = peek(memTL.get.d.bits.data)
    val corrupt = peek(memTL.get.d.bits.corrupt)

    DChannel(
      opcode = opcode,
      param = param,
      size = size,
      source = source,
      sink = sink,
      data = data,
      corrupt = corrupt != BigInt(0)
    )
  }

  def tlReadD(): DChannel = {
    poke(memTL.get.d.ready, 1)

    while (peek(memTL.get.d.valid) != BigInt(0)) {
      step(1)
    }
    val d = peekD()
    step(1)

    poke(memTL.get.d.ready, 0)
    d
  }

  def pokeE(e: EChannel): Unit = {
    poke(memTL.get.e.bits.sink, e.sink)
  }

  def tlWriteE(e: EChannel): Unit = {
    poke(memTL.get.e.valid, 1)
    pokeE(e)

    while (peek(memTL.get.e.ready) != BigInt(0)) {
      step(1)
    }
    step(1)
    poke(memTL.get.e.valid, 0)
  }

  def memWriteWord(addr: BigInt, data: BigInt): Unit = {
    tlWriteWord(addr, data)
  }
  def tlWriteWord(addr: BigInt, data: BigInt): Unit = {
    step(1)
    val a = tlWriteA(AChannel(opcode = 0 /* PUT */, address = addr, data = data, mask = BigInt("1" * 8, 2)))
  }

  def memWriteWord(addr: BigInt, data: BigInt, beatBytes: Int): Unit = {
    tlWriteWord(addr, data, beatBytes)
  }
  def tlWriteWord(addr: BigInt, data: BigInt, beatBytes: Int): Unit = {
    step(1)
    val a = tlWriteA(
      AChannel(
        opcode = 0 /* PUT */,
        address = addr,
        data = data,
        size = log2Ceil(beatBytes),
        mask = BigInt("1" * beatBytes, 2)
      )
    )
  }

  def tlWriteByte(addr: BigInt, data: Int): Unit = {
    val a = tlWriteA(AChannel(opcode = 0 /* PUT */, address = addr, data = data, mask = BigInt("1" * 8, 2)))
  }

  def tlWriteBytes(addr: BigInt, data: Seq[Int]): Unit = {
    data.zipWithIndex.foreach {
      case (d, i) =>
        tlWriteByte(addr + i, d)
    }
  }

  def memReadWord(addr: BigInt): BigInt = tlReadWord(addr)
  def tlReadWord(addr: BigInt): BigInt = {
    step(1)
    val d = tlWriteA(AChannel(opcode = 4 /* GET */, address = addr))
    d
  }

  def memReadWord(addr: BigInt, beatBytes: Int): BigInt = tlReadWord(addr, beatBytes)
  def tlReadWord(addr: BigInt, beatBytes: Int): BigInt = {
    step(1)
    val d = tlWriteA(
      AChannel(opcode = 4 /* GET */, address = addr, size = log2Ceil(beatBytes), mask = BigInt("1" * beatBytes, 2))
    )
    d
  }
}
