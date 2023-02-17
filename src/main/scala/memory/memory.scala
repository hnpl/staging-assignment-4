// The instruction and data memory modules

package dinocpu.memory

import chisel3._
import chisel3.util._
import MemoryOperation._

/**
  * This is the actual memory. You should never directly use this in the CPU.
  * This module should only be instantiated in the Top file.
  *
  * The I/O for this module is defined in [[MemPortBusIO]].
  */

class DualPortedCombinMemory(size: Int, memfile: String) extends BaseDualPortedMemory (size, memfile) {
  def wireMemPipe(portio: MemPortBusIO): Unit = {
    portio.response.valid := false.B
    // Combinational memory is inherently always ready for port requests
    portio.request.ready := true.B
  }

  // Instruction port

  wireMemPipe(io.inst_io)

  when (io.inst_io.request.valid) {
    // Put the Request into the instruction pipe and signal that instruction memory is busy
    val request = io.inst_io.request.bits

    // We should only be expecting a read from instruction memory
    assert(request.operation === Read)
    // Check that address is pointing to a valid location in memory

    // TODO: Revert this back to the assert form "assert (request.address < size.U)"
    // TODO: once CSR is integrated into CPU
    when (request.address < size.U) {
      io.inst_io.response.valid := true.B
      val baseAddress = (request.address >> 3.U) << 1.U
      io.inst_io.response.bits.data := Cat(memory(baseAddress + 1.U), memory(baseAddress))
      io.inst_io.response.bits.dirty := false.B
      io.inst_io.response.bits.age := 0.U
    } .otherwise {
      io.inst_io.response.valid := false.B
    }
  } .otherwise {
    io.inst_io.response.valid := false.B
  }

  // Data port

  wireMemPipe(io.data_io)

  val memAddress = io.data_io.request.bits.address
  val memWriteData = io.data_io.request.bits.writedata

  when (io.data_io.request.valid) {
    val request = io.data_io.request.bits

    // Check that non-combin write isn't being used
    assert (request.operation =/= Write)
    // Check that address is pointing to a valid location in memory
    assert (request.address < size.U)

    // Read path
    val baseAddress = memAddress >> 2.U
    io.data_io.response.bits.data := Cat(memory(baseAddress + 1.U), memory(baseAddress))
    io.data_io.response.bits.dirty := false.B
    io.data_io.response.bits.age := 0.U
    io.data_io.response.valid := true.B

    // Write path
    when (request.operation === ReadWrite) {
      memory(memAddress >> 2) := memWriteData(31, 0)
      memory((memAddress >> 2) + 1.U) := memWriteData(63, 32)
    }
    .otherwise {
    }
  } .otherwise {
    io.data_io.response.valid := false.B
  }
}
