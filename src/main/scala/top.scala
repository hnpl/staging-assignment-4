// Main entry point for CPU
package dinocpu

import chisel3._

class Top(val conf: CPUConfig) extends Module
{
  val io = IO(new Bundle{
    val success = Output(Bool())
  })

  io.success := DontCare

  val system_name = conf.getSystemName()

  val cpu  = Module(conf.getCPU())
  val l1_cache = if (system_name == "default") conf.getEmptyCache() else Module(conf.getL1Cache(1))
  val mem  = Module(conf.getNewMem())

  val inst_bridge = Module(conf.getIMemBridge())
  val data_bridge = Module(conf.getDMemBridge())

  conf.printConfig()

  cpu.io.imem <> inst_bridge.io.cpu_side_io
  cpu.io.dmem <> data_bridge.io.cpu_side_io

  system_name match {
    case "default" => build_default_system()
    case "system1" => build_system_1()
    case "system2" => build_system_2()
    case "system3" => build_system_3()
    case "system4" => build_system_4()
    case _ => throw new IllegalArgumentException(
      "Unknown system name. Choices: default, system1, system2, system3, system4."
    )
  }

  def build_default_system(): Unit = {
    mem.wireToInstBridge(inst_bridge)
    mem.wireToDataBridge(data_bridge)
  }

  // no cache
  def build_system_1(): Unit = {
    l1_cache.notUsingInstCache()
    l1_cache.notUsingDataCache()
    mem.wireToInstBridge(inst_bridge)
    mem.wireToDataBridge(data_bridge)
  }

  // inst cache only
  def build_system_2(): Unit = {
    l1_cache.wireToInstBridge(inst_bridge)
    l1_cache.notUsingDataCache()
    mem.wireToDataBridge(data_bridge)
    mem.wireInstPortsToHigherLevelMemory(l1_cache.io.inst_active_port_pair)
  }

  // data cache only
  def build_system_3(): Unit = {
    l1_cache.notUsingInstCache()
    l1_cache.wireToDataBridge(data_bridge)
    mem.wireToInstBridge(inst_bridge)
    mem.wireDataPortsToHigherLevelMemory(l1_cache.io.data_active_port_pair)
  }

  // both data and inst caches
  def build_system_4(): Unit = {
    l1_cache.wireToInstBridge(inst_bridge)
    l1_cache.wireToDataBridge(data_bridge)
    mem.wireInstPortsToHigherLevelMemory(l1_cache.io.inst_active_port_pair)
    mem.wireDataPortsToHigherLevelMemory(l1_cache.io.data_active_port_pair)
  }
}
