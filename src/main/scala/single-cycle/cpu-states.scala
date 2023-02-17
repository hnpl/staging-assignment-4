package dinocpu

import chisel3._
import chisel3.experimental.ChiselEnum

object SingleCycleNonCombinCPUState extends ChiselEnum {
  val WaitingForInst, WaitingForData = Value
}