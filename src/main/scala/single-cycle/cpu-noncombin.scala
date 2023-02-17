// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleNonCombinCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val controlTransfer = Module(new ControlTransferUnit())
  val hazard          = Module(new HazardSingleCycle())
  val (cycleCount, _) = Counter(true.B, 1 << 30)


  val instruction = Wire(UInt(32.W))
  val instructionCache = RegInit("h13".U(32.W))
  val state = RegInit(SingleCycleNonCombinCPUState.WaitingForInst)
  val isStalled = Wire(Bool())

  //FETCH
  io.imem.address := pc
  io.imem.valid := false.B

  when (state === SingleCycleNonCombinCPUState.WaitingForInst) {
    when (io.imem.good) { //received the instruction
      when ((pc % 8.U) === 4.U) {
        instruction := io.imem.instruction(63, 32)
        instructionCache := io.imem.instruction(63, 32)
      } .otherwise {
        instruction := io.imem.instruction(31, 0)
        instructionCache := io.imem.instruction(31, 0)
      }
      when (control.io.memop =/= 0.U) {
        state := SingleCycleNonCombinCPUState.WaitingForData
        isStalled := true.B
      }
      .otherwise {
        isStalled := false.B
        io.imem.valid := true.B
      }
    }
    .otherwise {
      instruction := "h13".U
      isStalled := true.B
      io.imem.valid := true.B
    }
  }
  .otherwise { //waiting for data
    when (io.dmem.good) {
      state := SingleCycleNonCombinCPUState.WaitingForInst
      instruction := instructionCache
      isStalled := false.B
    }
    .otherwise {
      //instruction := "h13".U
      instruction := instructionCache
      isStalled := true.B
    }
  }

  //when ((pc % 8.U) === 4.U) {
  //  instruction := io.imem.instruction(63, 32)
  //} .otherwise {
  //  instruction := io.imem.instruction(31, 0)
  //}
  val funct3 = instruction(14, 12)

  control.io.opcode := instruction(6, 0)

  registers.io.readreg1 := instruction(19, 15)
  registers.io.readreg2 := instruction(24, 20)
  registers.io.writereg := instruction(11, 7)
  registers.io.writedata := MuxCase(0.U, Array(
    (control.io.writeback_src === 0.U) -> alu.io.result,
    (control.io.writeback_src === 1.U) -> immGen.io.sextImm,
    (control.io.writeback_src === 2.U) -> io.dmem.readdata
  ))
  registers.io.wen := (registers.io.writereg =/= 0.U) && (control.io.writeback_valid === 1.U)

  immGen.io.instruction := instruction

  controlTransfer.io.controltransferop := control.io.controltransferop
  controlTransfer.io.operand1 := registers.io.readdata1
  controlTransfer.io.operand2 := alu.io.operand2
  controlTransfer.io.funct3 := funct3
  controlTransfer.io.pc := pc
  controlTransfer.io.imm := immGen.io.sextImm

  aluControl.io.aluop  := control.io.aluop
  aluControl.io.funct7 := instruction(31, 25)
  aluControl.io.funct3 := instruction(14, 12)

  alu.io.operation := aluControl.io.operation
  alu.io.operand1 := MuxCase(0.U, Array(
    (control.io.op1_src === 0.U) -> registers.io.readdata1,
    (control.io.op1_src === 1.U) -> pc
  ))
  alu.io.operand2 := MuxCase(0.U, Array(
    (control.io.op2_src === 0.U) -> registers.io.readdata2,
    (control.io.op2_src === 1.U) -> 4.U(64.W),
    (control.io.op2_src === 2.U) -> immGen.io.sextImm
  ))

  io.dmem.address := alu.io.result
  io.dmem.memread := control.io.memop === 1.U
  io.dmem.memwrite := control.io.memop === 2.U
  // send the instruction right when we have the instruction
  io.dmem.valid := (control.io.memop =/= 0.U) & (state === SingleCycleNonCombinCPUState.WaitingForData)
  io.dmem.maskmode := funct3(1, 0)
  io.dmem.sext := ~funct3(2)
  io.dmem.writedata := registers.io.readdata2

  hazard.io.memop := control.io.memop =/= 0.U
  hazard.io.imem_good := io.imem.good
  hazard.io.dmem_good := io.dmem.good

  when (!isStalled) {
    pc := controlTransfer.io.nextpc
  }
}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleNonCombinCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "nextpc",
      "hazard"
    )
  }
}
