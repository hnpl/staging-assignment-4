// This file contains the hazard detection unit

package dinocpu.components

import chisel3._

/**
 * The hazard detection unit
 *
 * Input:  memop, True if the instruction is a memory instruction
 * Input:  imem_good, True if imem has a response
 * Input:  dmem_good, True if dmem has a response
 *
 * Output: stall, True if it is necessary to stall the core
 */
class HazardSingleCycle extends Module {
  val io = IO(new Bundle {
    val memop = Input(Bool())
    val imem_good = Input(Bool())
    val dmem_good = Input(Bool())

    val stall = Output(Bool())
  })

  // We stall the CPU when the instruction is not ready, or the cpu is executing a memop and the response is not ready
  io.stall := (!io.imem_good) || (io.memop && !io.dmem_good)

  // Note: When we stall a single cycle cpu, we want to avoid 3 things,
  // - Write to PC
  // - Write to a register
  // - Change the memory request
}
