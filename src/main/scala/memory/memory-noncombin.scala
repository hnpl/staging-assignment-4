// Non-combinational ('asynchronous') memory module

package dinocpu.memory

import chisel3._
import chisel3.util._
import dinocpu.memory.MemoryOperation._

/**
 * The modified asynchronous form of the dual ported memory module.
 * When io.imem.request.valid or io.imem.request.valid is true and the memory is ready for an operation,
 * this memory module simulates the latency of real DRAM by pushing memory accesses into pipes that delay
 * the request for a configurable latency.
 *
 * As with the synced memory module, this memory should only be instantiated in the Top file,
 * and never within the actual CPU.
 *
 * The I/O for this module is defined in [[MemPortBusIO]].
 */
class DualPortedNonCombinMemory(size: Int, memfile: String, latency: Int) extends BaseDualPortedMemory(size, memfile) {
  def wireMemPipe(portio: MemPortBusIO, pipe: Pipe[Request]): Unit = {
    pipe.io.enq.bits      <> DontCare
    pipe.io.enq.valid     := false.B
    portio.response.valid := false.B
  }
  assert(latency > 0) // Check for attempt to make combinational memory

  val instMemState = RegInit(NonCombinMemoryState.Ready)
  val dataMemState = RegInit(NonCombinMemoryState.Ready)

  io.inst_io.request.ready := instMemState === NonCombinMemoryState.Ready
  io.data_io.request.ready := dataMemState === NonCombinMemoryState.Ready

  // Instruction port
  val imemPipe = Module(new Pipe(new Request, latency))

  wireMemPipe(io.inst_io, imemPipe)

  when (instMemState === NonCombinMemoryState.Ready) {
    when (io.inst_io.request.valid) { // accepting incoming request
      // Put the Request into the instruction pipe and signal that instruction memory is busy
      val inRequest = io.inst_io.request.bits
      imemPipe.io.enq.bits  := inRequest
      imemPipe.io.enq.valid := true.B
      instMemState := NonCombinMemoryState.Busy
    } .otherwise {
      imemPipe.io.enq.valid := false.B
    }
  }
  .elsewhen (instMemState === NonCombinMemoryState.Busy) {
    when (imemPipe.io.deq.valid) {
      // We should only be expecting a read from instruction memory
      assert(imemPipe.io.deq.bits.operation === Read)
      val outRequest = imemPipe.io.deq.bits
      // Check that address is pointing to a valid location in memory
      assert (outRequest.address < size.U)
      io.inst_io.response.valid        := true.B
      val baseAddress = (imemPipe.io.deq.bits.address >> 3.U) << 1.U
      io.inst_io.response.bits.data := Cat(memory(baseAddress+1.U), memory(baseAddress))
      io.inst_io.response.bits.dirty := false.B
      io.inst_io.response.bits.age := 0.U
      instMemState := NonCombinMemoryState.Ready
    } .otherwise {
      // The memory's response can't possibly be valid if the imem pipe isn't outputting a valid request
      io.inst_io.response.valid := false.B
    }
  }
  .otherwise {
    // should not be here
  }

  // Data port

  val dmemPipe     = Module(new Pipe(new Request, latency))

  wireMemPipe(io.data_io, dmemPipe)

  when (dataMemState === NonCombinMemoryState.Ready) {
    when (io.data_io.request.valid) {
      // Put the Request into the data pipe and signal that data memory is busy
      val inRequest = io.data_io.request.bits
      dmemPipe.io.enq.bits  := inRequest
      dmemPipe.io.enq.valid := true.B
      dataMemState := NonCombinMemoryState.Busy
    } .otherwise {
      dmemPipe.io.enq.valid := false.B
    }
  }
  .elsewhen (dataMemState === NonCombinMemoryState.Busy) {
    when (dmemPipe.io.deq.valid) {
      assert (dmemPipe.io.deq.bits.operation =/= ReadWrite)
      // Dequeue request and execute
      val outRequest = dmemPipe.io.deq.bits
      val address = outRequest.address >> 2
      // Check that address is pointing to a valid location in memory
      assert (outRequest.address < size.U)

      when (outRequest.operation === Read) {
        io.data_io.response.valid        := true.B
        io.data_io.response.bits.data    := Cat(memory(address + 1.U), memory(address))
      } .elsewhen (outRequest.operation === Write) {
        io.data_io.response.valid        := true.B
        memory(address) := outRequest.writedata(31, 0)
        memory(address + 1.U) := outRequest.writedata(63, 32)
      }
      io.data_io.response.bits.dirty := false.B
      io.data_io.response.bits.age := 0.U
      dataMemState := NonCombinMemoryState.Ready
    } .otherwise {
      // The memory's response can't possibly be valid if the dmem pipe isn't outputting a valid request
      io.data_io.response.valid := false.B
    }
  }
}
