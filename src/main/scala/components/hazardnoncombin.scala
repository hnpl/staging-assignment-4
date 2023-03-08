// This file contains the hazard detection unit

package dinocpu.components

import chisel3._

/**
 * The hazard detection unit
 *
 * Input:  rs1, the first source register number
 * Input:  rs2, the second source register number
 * Input:  idex_memread, true if the instruction in the ID/EX register is going to read from memory
 * Input:  idex_rd, the register number of the destination register for the instruction in the ID/EX register
 * Input:  exmem_taken, if true, then we are using the nextpc in the EX/MEM register, *not* pc+4.
 * Input:  exmem_meminst, if true, the instruction at MEM stage is a memory instruction.
 * Input:  imem_ready, if true, then the Instruction Memory is ready for another instruction 
 * Input:  imem_good, if true, then an instruction was successfully retrieved and can unstall CPU
 * Input:  dmem_good, if true, then can unstall CPU for data memory
 *
 * Output: pcfromtaken, if true, use the pc from MEM
 * Output: pcstall, if true, stall the pipeline
 * Output: if_id_stall, if true, stall the if_id register. 
 * Output: if_id_flush, if true, flush the if_id register. 
 * Output: id_ex_stall, if true, stall the id_ex register. 
 * Output: id_ex_flush, if true, flush the id_ex register. 
 * Output: ex_mem_stall, if true, stall the ex_mem register. 
 * Output: ex_mem_flush, if true, flush the ex_mem register. 
 * Output: mem_wb_stall, if true, stall the mem_wb register. 
 * Output: mem_wb_flush, if true, flush the mem_wb register. 
 * Output: imem_valid, if true, imem can process new instruction
 *
 * For more information, see Section 4.7 and beginning of 4.8 of Patterson and Hennessy
 * This follows the "Data hazards and stalls" section and the "Assume branch not taken" section
 */
class HazardUnitNonCombin extends Module {
  val io = IO(new Bundle {
    val rs1           = Input(UInt(5.W))
    val rs2           = Input(UInt(5.W))
    val idex_memread  = Input(Bool())
    val idex_rd       = Input(UInt(5.W))
    val exmem_taken   = Input(Bool())
    val exmem_meminst = Input(Bool())
    val imem_ready    = Input(Bool())
    val imem_good     = Input(Bool())
    val dmem_good     = Input(Bool())

    val pcfromtaken  = Output(Bool())
    val pcstall      = Output(Bool())
    val if_id_stall  = Output(Bool())
    val if_id_flush  = Output(Bool())
    val id_ex_stall  = Output(Bool())
    val id_ex_flush  = Output(Bool())
    val ex_mem_stall = Output(Bool())
    val ex_mem_flush = Output(Bool())
    val mem_wb_stall = Output(Bool())
    val mem_wb_flush = Output(Bool())
  })

  // default
  io.pcfromtaken  := false.B
  io.pcstall      := false.B
  io.if_id_stall  := false.B
  io.if_id_flush  := false.B
  io.id_ex_stall  := false.B
  io.id_ex_flush  := false.B
  io.ex_mem_stall := false.B
  io.ex_mem_flush := false.B
  io.mem_wb_stall := false.B
  io.mem_wb_flush := false.B

  //                                     |  IF |  ID |  EX | MEM |  WB |
  // Condition 1: exmem_taken is true    |-----|flush|flush|flush|-----|
  // Condition 2: load-to-use            |stall|stall|flush|-----|-----|
  // Condition 3: exmem_meminst is true  |stall|stall|stall|stall|-----|
  //              and !dmem_good
  // Condition 4: !imem_good             |stall|flush|-----|-----|-----|
  // ---
  // Condition 1 and condition 3 are mutually exclusive.
  // | Case | Cond 1 | Cond 2 | Cond 3 | Cond 4 |  nextpc |  ID |  EX | MEM |  WB |
  // |------|--------|--------|--------|--------|---------|-----|-----|-----|-----|
  // Have new instruction in IF
  // |    1 |      0 |      0 |      0 |      0 |    pc+4 |-----|-----|-----|-----|
  // |    2 |      0 |      0 |      1 |      0 | pcstall |stall|stall|stall|flush| // because the dmem is busy, we have to discard the correct instruction and retry fetching it ...
  // |    3 |      0 |      1 |      0 |      0 | pcstall |stall|flush|-----|-----|
  // |    4 |      0 |      1 |      1 |      0 | pcstall |stall|stall|stall|flush|
  // |    5 |      1 |      0 |      0 |      0 |    pc+4 |flush|flush|flush|-----| // move the branch/jump to WB, the takenpc is valid
  // |    6 |      1 |      1 |      0 |      0 |    pc+4 |flush|flush|flush|-----| // same as above
  // Not have new instruction in IF
  // |    7 |      0 |      0 |      0 |      1 | pcstall |flush|-----|-----|-----|
  // |    8 |      0 |      0 |      1 |      1 | pcstall |stall|stall|stall|flush|
  // |    9 |      0 |      1 |      0 |      1 | pcstall |stall|flush|-----|-----|
  // |   10 |      0 |      1 |      1 |      1 | pcstall |stall|stall|stall|flush| // stall till we can move branch/jump from EX to MEM
  // |   11 |      1 |      0 |      0 |      1 | takenpc |flush|flush|stall|flush| // stall till we can discard current inst req
  // |   12 |      1 |      1 |      0 |      1 | takenpc |flush|flush|stall|flush| // same as above

  val cond1 = io.exmem_taken
  val cond2 = io.idex_memread & (io.idex_rd =/= 0.U) & (io.idex_rd === io.rs1 | io.idex_rd === io.rs2)
  val cond3 = io.exmem_meminst & (io.dmem_good =/= 1.U)
  val cond4 = !io.imem_good

  when (cond3) { // case 2, 4, 8, 10
    io.pcstall := true.B
    io.if_id_stall := true.B
    io.id_ex_stall := true.B
    io.ex_mem_stall := true.B
    io.mem_wb_flush := true.B
  } .elsewhen (cond2) { // !(cond3) & cond2 // case 3, 9
    io.pcstall := true.B
    io.if_id_stall := true.B
    io.id_ex_flush := true.B
  } .elsewhen (cond1) { // case 5, 6, 11, 12, cond4 should differentiate case 5/6 and case 11/12
    io.pcfromtaken := cond4
    io.if_id_flush := cond4
    io.id_ex_flush := true.B
    io.ex_mem_stall := cond4
    io.mem_wb_flush := cond4
  } .elsewhen (cond4) { // case 7
    io.pcstall := true.B
    io.if_id_flush := true.B
  }

}
