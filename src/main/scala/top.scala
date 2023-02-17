// Main entry point for CPU
package dinocpu

import chisel3._

class Top(val conf: CPUConfig) extends Module
{
  val io = IO(new Bundle{
    val success = Output(Bool())
  })

  io.success := DontCare

  val cpu  = Module(conf.getCPU())
  val l1_cache = Module(conf.getL1Cache(1))
  val mem  = Module(conf.getNewMem())

  val inst_bridge = Module(conf.getIMemBridge())
  val data_bridge = Module(conf.getDMemBridge())

  conf.printConfig()

  cpu.io.imem <> inst_bridge.io.cpu_side_io
  cpu.io.dmem <> data_bridge.io.cpu_side_io

  // no cache
  l1_cache.notUsingInstCache()
  l1_cache.notUsingDataCache()
  mem.wireToInstBridge(inst_bridge)
  mem.wireToDataBridge(data_bridge)

  // inst cache only
  //l1_cache.wireToInstBridge(inst_bridge)
  //l1_cache.notUsingDataCache()
  //mem.wireToDataBridge(data_bridge)
  //mem.wireInstPortsToHigherLevelMemory(l1_cache.io.inst_active_port_pair)

  // data cache only
  //l1_cache.notUsingInstCache()
  //l1_cache.wireToDataBridge(data_bridge)
  //mem.wireToInstBridge(inst_bridge)
  //mem.wireDataPortsToHigherLevelMemory(l1_cache.io.data_active_port_pair)

  // both data and inst caches
  //l1_cache.wireToInstBridge(inst_bridge)
  //l1_cache.wireToDataBridge(data_bridge)
  //mem.wireInstPortsToHigherLevelMemory(l1_cache.io.inst_active_port_pair)
  //mem.wireDataPortsToHigherLevelMemory(l1_cache.io.data_active_port_pair)
}
