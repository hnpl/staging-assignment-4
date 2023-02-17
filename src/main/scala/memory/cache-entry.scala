package dinocpu.memory

import chisel3._

class CacheEntry(val numEntryIndexingBits:Int, val numTagBits: Int) extends Bundle {
  val valid  = Bool()
  val dirty  = Bool()
  val tag    = UInt(numTagBits.W)
  val memIdx = UInt(numEntryIndexingBits.W) // where is it in the cache memory
  val age    = UInt(32.W)
}