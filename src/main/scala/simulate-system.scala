// Main entry point for simulation
package dinocpu

import dinocpu.test._

import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}
import treadle.{HasTreadleOptions, TreadleOptionsManager, TreadleTester}
import java.io.{File, PrintWriter, RandomAccessFile}

import chisel3.{ChiselExecutionFailure,ChiselExecutionSuccess,HasChiselExecutionOptions}
import net.fornwall.jelf.ElfFile

import scala.collection.SortedMap
import scala.util.control.NonFatal

/**
 * Simple object with only a main function to run the treadle simulation.
 * When run, this will begin execution and continue until the PC reaches the
 * "_last" symbol in the elf file or the max cycle parameter is reached
 *
 * {{{
 *  sbt> runMain dinocpu.simulate_predefined_system <system_name> <riscv_binary>
 * }}}
 */
object simulate_predefined_system {
  val helptext = "usage: simulate_predefined_system <system_name> <riscv_binary>"

  def elfToHex(filename: String, outfile: String) = {
    val elf = ElfFile.fromFile(new java.io.File(filename))
    val sections = Seq(".text", ".data") // These are the sections we want to pull out
    // address to put the data -> offset into the binary, size of section
    var info = SortedMap[Long, (Long, Long)]()
    // Search for these section names
    for (i <- 1 until elf.num_sh) {
      val section =  elf.getSection(i)
      if (sections.contains(section.getName)) {
        info += section.address -> (section.section_offset, section.size)
      }
    }

    // Now, we want to create a new file to load into our memory
    val output = new PrintWriter(new File(outfile))
    val f = new RandomAccessFile(filename, "r")
    var location = 0
    for ((address, (offset, size)) <- info) {
      while (location < address) {
        require(location + 3 < address, "Assuming addresses aligned to 4 bytes")
        output.write("00000000\n")
        location += 4
      }
      val data = new Array[Byte](size.toInt)
      f.seek(offset)
      f.read(data)
      var s = List[String]()
      for (byte <- data) {
        s = s :+ ("%02X" format byte)
        location += 1
        if (location % 4 == 0) {
          // Once we've read 4 bytes, swap endianness
          output.write(s(3)+s(2)+s(1)+s(0)+"\n")
          s = List[String]()
        }
      }
    }
    output.close()
    // Return the final PC value we're looking for
    val symbol = elf.getELFSymbol("_last")

    if (symbol != null) symbol.value
    else 0x400L
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 2, "Error: Expected exactly two arguments\n" + helptext)

    val optionsManager = new SimulatorOptionsManager

    if (optionsManager.parser.parse(args)) {
      optionsManager.setTargetDirName("simulator_run_dir")
    } else {
      None
    }

    // Get the name for the hex file
    val hexName = optionsManager.targetDirName + "/executable.hex"

    // Create the CPU config. This sets the type of CPU and the binary to load
    val conf = new CPUConfig()

    val test = InstTests.nameMap(args(1))
    val (cpuType, memType, memPortType) =
        ("pipelined-non-combin", "non-combinational", "non-combinational-port")

    val systemName = args(0)
    val memoryLatency = systemName match {
      case "default" => 1
      case "system1" => 30
      case "system2" => 30
      case "system3" => 30
      case "system4" => 30
    }

    val driver = new CPUTesterDriver(
      cpuType, "", test.binary, test.extraName, memType, memPortType, memoryLatency, systemName
    )
    driver.initRegs(test.initRegs)
    driver.initMemory(test.initMem)

    val cycles = 10000000
    driver.run(cycles)
    if (driver.checkRegs(test.checkRegs) && driver.checkMemory(test.checkMem)) {
      println("Test passed!")
    } else {
      println("Test failed!")
    }

    println(s"Number of cycles: ${driver.cycle}")

    try {
      driver.reportCacheStats()
    } catch {
      case e: AssertionError => None
    }

  }
}

