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
  val mem  = Module(conf.getNewMem())

  val inst_bridge = Module(conf.getIMemBridge())
  val data_bridge = Module(conf.getDMemBridge())

  conf.printConfig()

  cpu.io.imem <> inst_bridge.io.cpu_side_io
  cpu.io.dmem <> data_bridge.io.cpu_side_io

  mem.wireToInstBridge(inst_bridge)
  mem.wireToDataBridge(data_bridge)
}
