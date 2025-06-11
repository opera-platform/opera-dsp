package freechips.rocketchip.amba.axi4stream

import chisel3.Module
import chiseltest.iotesters.PeekPokeTester

import scala.language.implicitConversions


class AXI4StreamRandomPeekPokeMaster(port: AXI4StreamBundle, tester: PeekPokeTester[_], random: Boolean = false) {
  protected var input: Seq[AXI4StreamTransaction] = Seq()

  def addTransactions(in: Seq[AXI4StreamTransaction]): Unit = {
    input ++= in
  }

  def step(): Unit = {
    import tester.{peek, poke}
    if (input.isEmpty) {
      poke(port.valid, 0)
    } else {
      val valid = BigInt(if (random) scala.util.Random.nextInt(2) else 1)
      val t = input.head
      poke(port.valid, valid)
      poke(port.bits.data, t.data)
      poke(port.bits.last, if (t.last) 1 else 0)
      if (port.bits.strb.getWidth > 0) {
        if (t.strb == -1) {
          val allOnes = (BigInt(1) << port.bits.strb.getWidth) - 1
          poke(port.bits.strb, allOnes)
        } else {
          poke(port.bits.strb, t.strb)
        }
      }
      if (port.bits.keep.getWidth > 0) {
        if (t.keep == -1) {
          val allOnes = (BigInt(1) << port.bits.keep.getWidth) - 1
          poke(port.bits.keep, allOnes)
        } else {
          poke(port.bits.keep, t.keep)
        }
      }
      if (port.bits.user.getWidth > 0) {
        poke(port.bits.user, t.user)
      }
      if (port.bits.id.getWidth > 0) {
        poke(port.bits.id, t.id)
      }
      if (port.bits.dest.getWidth > 0) {
        poke(port.bits.dest, t.dest)
      }
      if (peek(port.ready) != BigInt(0) & peek(port.valid) != BigInt(0)) {
        input = input.tail
      }
    }
  }

  def complete(): Boolean = {
    input.isEmpty
  }
}

trait AXI4StreamRandomMasterModel[T <: Module] extends PeekPokeTester[T] {
  protected var masters: Seq[AXI4StreamRandomPeekPokeMaster] = Seq()

  def resetMaster(port: AXI4StreamBundle): Unit = {
    poke(port.valid, 0)
  }

  def bindMaster(port: AXI4StreamBundle, random: Boolean = false): AXI4StreamRandomPeekPokeMaster = {
    resetMaster(port)
    val master = new AXI4StreamRandomPeekPokeMaster(port, this, random)
    masters +:= master
    master
  }

  protected def stepMasters(): Unit = {
    masters.foreach(_.step())
  }

  override def step(n: Int): Unit = {
    for (_ <- 0 until n) {
      stepMasters()
      super.step(1)
    }
  }

  def mastersComplete(): Boolean = {
    masters.map(_.complete()).forall(x => x)
  }

  def stepToCompletion(maxCycles: Int = 1000, silentFail: Boolean = false): Unit = {
    for (_ <- 0 until maxCycles) {
      if (mastersComplete()) {
        step(1)
        return
      } else {
        step(1)
      }
    }
    require(silentFail, s"slavesComplete: ${mastersComplete()}")
  }
}