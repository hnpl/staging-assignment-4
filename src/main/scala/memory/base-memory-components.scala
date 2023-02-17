// Base memory classes

package dinocpu.memory

import dinocpu.memory.MemoryOperation._
import chisel3._
import chisel3.util.{Decoupled, Valid}
import chisel3.util.experimental.loadMemoryFromFile

/**
  * Base class for all modular backing memories. Simply declares the IO and the memory file.
  */
abstract class BaseDualPortedMemory(size: Int, memfile: String) extends Module {
  def wireToInstBridge(inst_bridge: BaseIMemBridge): Unit = {
    // Connect memory imem IO to dmem accessor
    this.io.inst_io.request <> inst_bridge.io.mem_side_io.request
    inst_bridge.io.mem_side_io.response <> this.io.inst_io.response
  }

  def wireToDataBridge(data_bridge: BaseDMemBridge): Unit = {
    // Connect memory dmem IO to dmem accessor
    this.io.data_io.request <> data_bridge.io.mem_side_io.request
    data_bridge.io.mem_side_io.response <> this.io.data_io.response
  }

  def wireInstPortsToHigherLevelMemory(other_inst_active_port_pair: MemPortBusIO): Unit = {
    this.io.inst_io.request <> other_inst_active_port_pair.request
    this.io.inst_io.response <> other_inst_active_port_pair.response
  }

  def wireDataPortsToHigherLevelMemory(other_data_active_port_pair: MemPortBusIO): Unit = {
    this.io.data_io.request <> other_data_active_port_pair.request
    this.io.data_io.response <> other_data_active_port_pair.response
  }

  val io = IO(new Bundle {
    val inst_io = new MemPortBusIO
    val data_io = new MemPortBusIO
  })

  // Intentional DontCares:
  // The connections between the ports and the backing memory, along with the
  // ports internally assigning values to the, means that these DontCares
  // should be completely 'overwritten' when the CPU is elaborated
  io.inst_io.request <> DontCare
  io.data_io.request <> DontCare
  // Zero out response ports to 0, so that the pipeline does not receive any
  // 'DontCare' values from the memory ports
  io.inst_io.response <> 0.U.asTypeOf(Valid (new Response))
  io.data_io.response <> 0.U.asTypeOf(Valid (new Response))

  val memory = Mem(math.ceil(size.toDouble/4).toInt, UInt(32.W))
  loadMemoryFromFile(memory, memfile)
}

/**
  * Base class for all instruction ports. Simply declares the IO.
  */
abstract class BaseIMemBridge extends Module {
  val io = IO (new Bundle {
    val cpu_side_io = new IMemPortIO
    val mem_side_io  = Flipped (new MemPortBusIO)
  })

  io.cpu_side_io <> 0.U.asTypeOf (new IMemPortIO)
  // Intentional DontCare:
  // The connections between the ports and the backing memory, along with the
  // ports internally assigning values to the, means that these DontCares
  // should be completely 'overwritten' when the CPU is elaborated
  io.mem_side_io      <> DontCare
}

/**
  * Base class for all data ports. Simply declares the IO.
  */
abstract class BaseDMemBridge extends Module {
  val io = IO (new Bundle {
    val cpu_side_io = new DMemPortIO
    val mem_side_io = Flipped (new MemPortBusIO)
  })

  io.cpu_side_io <> 0.U.asTypeOf (new DMemPortIO)
  // Intentional DontCare:
  // The connections between the ports and the backing memory, along with the
  // ports internally assigning values to the, means that these DontCares
  // should be completely 'overwritten' when the CPU is elaborated
  io.mem_side_io      <> DontCare

  io.cpu_side_io.good := io.mem_side_io.response.valid
}
