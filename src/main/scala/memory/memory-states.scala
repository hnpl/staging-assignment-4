// Asynchronous memory module

package dinocpu.memory

import chisel3._
import chisel3.experimental.ChiselEnum

/**
 * Chisel enumerator to assign names to the UInt constants representing memory operations
 */

object ReadOnlyMemoryComponentState extends ChiselEnum {
  val Ready, BusyReading, BusyRetrying, BusyWriting, WaitingForResponse, HasResponse = Value
}

object ReadWriteMemoryComponentState extends ChiselEnum {
  val Ready, BusyReadingCache, BusyWritingCache, WaitingForResponse, BusyRetrying, DirtyEvicting, BusyRetryingEvicting, HasResponse = Value
}

object NonCombinMemoryState extends ChiselEnum {
  val Ready, Busy = Value
}

object DMemBridgeState extends ChiselEnum {
  val Idle, WaitingForLoad, WaitingForStore = Value
}