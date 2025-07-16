package freechips.rocketchip.amba.axi4

import chisel3._
import chisel3.util.IrrevocableIO
import chiseltest.iotesters._
import dspblocks.MemMasterModel
import freechips.rocketchip.util.BundleMap

trait OptionalAXI4MasterModel extends MemMasterModel {
  self: PeekPokeTester[_] =>
  import AXI4MasterModel._

  def memAXI: Option[AXI4Bundle]

  def maxWait = 500

  def fire(io: IrrevocableIO[_]): Boolean = {
    (peek(io.valid) != BigInt(0)) && (peek(io.ready) != BigInt(0))
  }

  def pokeUser(user: BundleMap, values: Map[String, BigInt]): Unit = {
    for ((k, v) <- values) {
      user.elements.get(k) match {
        case Some(Pokeable(e)) =>
          poke(e, v)
        case Some(b: Bundle) =>
          poke(b, values)
        case Some(d) =>
          println(s"Don't know how to poke element $d")
        case None =>
          println(s"user field $k not found")
      }
    }
  }

  def pokeAW(aw: AXI4BundleAW, value: AWChannel): Unit = {
    poke(aw.id, value.id)
    poke(aw.addr, value.addr)
    poke(aw.len, value.len)
    poke(aw.size, value.size)
    poke(aw.burst, value.burst)
    poke(aw.lock, value.lock)
    poke(aw.cache, value.cache)
    poke(aw.prot, value.prot)
    poke(aw.qos, value.qos)
    // poke(aw., value.region)
    require(
      value.region == BigInt(0),
      s"region is optional and rocket-chip left it out. overriding the default value here with ${value.region} won't do anything"
    )
    pokeUser(aw.user, value.user)
  }

  def pokeAR(ar: AXI4BundleAR, value: ARChannel): Unit = {
    poke(ar.id, value.id)
    poke(ar.addr, value.addr)
    poke(ar.len, value.len)
    poke(ar.size, value.size)
    poke(ar.burst, value.burst)
    poke(ar.lock, value.lock)
    poke(ar.cache, value.cache)
    poke(ar.prot, value.prot)
    poke(ar.qos, value.qos)
    pokeUser(ar.user, value.user)
  }

  def pokeW(w: AXI4BundleW, value: WChannel): Unit = {
    poke(w.data, value.data)
    poke(w.strb, value.strb)
    poke(w.last, value.last)
  }

  def peekR(r: AXI4BundleR): RChannel = {
    RChannel(
      id = peek(r.id),
      data = peek(r.data),
      resp = peek(r.resp),
      last = peek(r.last),
      user = r.user.elements.map { case (n: String, Pokeable(d)) => n -> peek(d) }
    )
  }

  def peekB(b: AXI4BundleB): BChannel = {
    BChannel(
      id = peek(b.id),
      resp = peek(b.resp),
      user = b.user.elements.map { case (n: String, Pokeable(d)) => n -> peek(d) }
    )
  }

  def memWriteWord(addr: BigInt, data: BigInt): Unit = axiWriteWord(addr, data)
  def axiWriteWord(addr: BigInt, data: BigInt): Unit = {
    val awChannel = AWChannel(
      addr = addr,
      size = 3 // 8 bytes
    )
    val wChannel = WChannel(
      data = data,
      strb = 0xff, // 8 bytes
      last = true // one word only
    )

    // poke AW and W channels
    pokeAW(memAXI.get.aw.bits, awChannel)
    pokeW(memAXI.get.w.bits, wChannel)

    var awFinished = false
    var wFinished = false
    var cyclesWaited = 0

    poke(memAXI.get.aw.valid, 1)
    poke(memAXI.get.w.valid, 1)

    while (!awFinished || !wFinished) {
      if (!awFinished) { awFinished = fire(memAXI.get.aw) }
      if (!wFinished) { wFinished = fire(memAXI.get.w) }
      require(cyclesWaited < maxWait || awFinished, s"Timeout waiting for AW to be ready ($maxWait cycles)")
      require(cyclesWaited < maxWait || wFinished, s"Timeout waiting for W to be ready ($maxWait cycles)")
      cyclesWaited += 1
      step(1)
      if (awFinished) { poke(memAXI.get.aw.valid, 0) }
      if (wFinished) { poke(memAXI.get.w.valid, 0) }
    }

    // wait for resp
    cyclesWaited = 0
    poke(memAXI.get.b.ready, 1)
    var bFinished = false
    var b = peekB(memAXI.get.b.bits)

    while (!bFinished) {
      bFinished = peek(memAXI.get.b.valid) != BigInt(0)
      b = peekB(memAXI.get.b.bits)
      require(cyclesWaited < maxWait, s"Timeout waiting for B to be valid ($maxWait cycles)")
      step(1)
      cyclesWaited += 1
    }

    poke(memAXI.get.b.ready, 0)

    require(b.id == awChannel.id, s"Got bad id (${b.id} != ${awChannel.id})")
    require(b.resp == BRESP_OKAY, s"BRESP not OKAY (got ${b.resp}")

  }
  def memReadWord(addr: BigInt) = axiReadWord(addr)
  def axiReadWord(addr: BigInt): BigInt = {
    val arChannel = ARChannel(
      addr = addr,
      size = 3 // 8 bytes
    )

    pokeAR(memAXI.get.ar.bits, arChannel)
    poke(memAXI.get.ar.valid, 1)

    var cyclesWaited = 0
    var arFinished = false

    while (!arFinished) {
      arFinished = peek(memAXI.get.ar.ready) != BigInt(0)
      require(cyclesWaited < maxWait, s"Timeout waiting for AR to be ready ($maxWait cycles)")
      step(1)
      cyclesWaited += 1
    }

    poke(memAXI.get.ar.valid, 0)
    poke(memAXI.get.r.ready, 1)

    var rFinished = false
    cyclesWaited = 0
    var rChannel = peekR(memAXI.get.r.bits)

    while (!rFinished) {
      poke(memAXI.get.ar.valid, 0)
      rFinished = peek(memAXI.get.r.valid) != BigInt(0)
      if (rFinished) {
        rChannel = peekR(memAXI.get.r.bits)
      }
      step(1)
      require(cyclesWaited < maxWait, s"Timeout waiting for R to be valid ($maxWait cycles)")

      cyclesWaited += 1
    }

    poke(memAXI.get.r.ready, 0)

    require(rChannel.last != BigInt(0))
    require(rChannel.id == arChannel.id, s"Got id ${rChannel.id} instead of ${arChannel.id}")
    require(rChannel.resp == RRESP_OKAY, s"RRESP not OKAY (got ${rChannel.resp}")
    rChannel.data
  }

  def axiReset(): Unit = {
    pokeAR(memAXI.get.ar.bits, ARChannel())
    pokeAW(memAXI.get.aw.bits, AWChannel())
    pokeW(memAXI.get.w.bits, WChannel())
    poke(memAXI.get.ar.valid, 0)
    poke(memAXI.get.aw.valid, 0)
    poke(memAXI.get.w.valid, 0)
    poke(memAXI.get.r.ready, 0)
    poke(memAXI.get.b.ready, 0)
  }
}
