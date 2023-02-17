// Non-combinational/'asynchronous' memory module

package dinocpu.memory

import chisel3._
import chisel3.util._
import dinocpu.memory.MemoryOperation._

// A Bundle used for temporarily storing the necessary information for a  read/write in the data memory accessor.
class OutstandingReq extends Bundle {
  val address   = UInt(64.W)
  val writedata = UInt(64.W)
  val maskmode  = UInt(2.W)
  val operation = MemoryOperation()
  val sext      = Bool()
}

/**
 * The instruction memory port. Since both the combinational and noncombinational instruction ports just issue
 * read requests in the same way both ports have the same implementation
 *
 * The I/O for this module is defined in [[IMemPortIO]].
 */
class INonCombinMemBridge extends ICombinMemBridge {
  io.cpu_side_io.good := io.mem_side_io.response.valid
  io.cpu_side_io.ready := io.mem_side_io.request.ready
}

/**
 * The data memory port.
 *
 * The I/O for this module is defined in [[DMemPortIO]].
 */
class DNonCombinMemBridge extends BaseDMemBridge {

  val dmemBridgeState = RegInit(DMemBridgeState.Idle)
  val receivedRequest = RegInit(0.U.asTypeOf(new OutstandingReq))
  val sendingRequest = RegInit(0.U.asTypeOf(new Request))

  when (dmemBridgeState === DMemBridgeState.Idle) {
    //io.cpu_side_io.ready := io.mem_side_io.request.ready // CPU must be aware of the ready signal
                                                         // memory only accepts a request if the CPU request is valid and memory is ready
    io.cpu_side_io.good := false.B
    when (io.cpu_side_io.valid & io.mem_side_io.request.ready) { // receiving a request
      receivedRequest.address := io.cpu_side_io.address
      receivedRequest.writedata := io.cpu_side_io.writedata
      receivedRequest.maskmode := io.cpu_side_io.maskmode
      when (io.cpu_side_io.memread) { receivedRequest.operation := MemoryOperation.Read }
      .otherwise { receivedRequest.operation := MemoryOperation.Write }
      receivedRequest.sext := io.cpu_side_io.sext

      // issue a load request first regardless
      // for store operation, we need to load first because the store can be of size smaller than a cache block
      dmemBridgeState := DMemBridgeState.WaitingForLoad
      // - formulate a load request
      io.mem_side_io.request.valid := true.B
      io.mem_side_io.request.bits.address := (io.cpu_side_io.address >> 3.U) << 3.U
      io.mem_side_io.request.bits.operation := MemoryOperation.Read
      io.mem_side_io.request.bits.writedata := 0.U
      io.mem_side_io.request.bits.dirty := 0.U
      io.mem_side_io.request.bits.age := 0.U
    }
    .otherwise {
      io.mem_side_io.request.valid := false.B
    }
  }
  .elsewhen (dmemBridgeState === DMemBridgeState.WaitingForLoad) {
    //io.cpu_side_io.ready := false.B
    io.cpu_side_io.good := false.B
    when (io.mem_side_io.response.valid) {
      when (receivedRequest.operation === MemoryOperation.Read) { // if it's a load, transition to Idle
        io.cpu_side_io.good := io.mem_side_io.response.valid
        dmemBridgeState := DMemBridgeState.Idle

        // do masking the data
        val readdata_mask      = Wire(UInt(64.W))
        val readdata_mask_sext = Wire(UInt(64.W))

        val offset = receivedRequest.address(2, 0)
        when (receivedRequest.maskmode === 0.U) { // Byte
          readdata_mask := (io.mem_side_io.response.bits.data >> (offset * 8.U)) & 0xff.U
        } .elsewhen (receivedRequest.maskmode === 1.U) { // Half-word
          readdata_mask := (io.mem_side_io.response.bits.data >> (offset * 8.U)) & 0xffff.U
        } .elsewhen (receivedRequest.maskmode === 2.U) { // Word
          readdata_mask := (io.mem_side_io.response.bits.data >> (offset * 8.U)) & 0xffffffffL.U
        } .otherwise { // Double-word
          readdata_mask := io.mem_side_io.response.bits.data
        }

        when (receivedRequest.sext) {
          when (receivedRequest.maskmode === 0.U) { // Byte sign extension
            readdata_mask_sext := Cat(Fill(56, readdata_mask(7)),  readdata_mask(7, 0))
          } .elsewhen (receivedRequest.maskmode === 1.U) { // Half-word sign extension
            readdata_mask_sext := Cat(Fill(48, readdata_mask(15)), readdata_mask(15, 0))
          } .elsewhen (receivedRequest.maskmode === 2.U) { // Word sign extension
            readdata_mask_sext := Cat(Fill(32, readdata_mask(31)), readdata_mask(31, 0))
          } .otherwise { // Double-word sign extension (does nothing)
            readdata_mask_sext := readdata_mask
          }
        } .otherwise {
          readdata_mask_sext := readdata_mask
        }
        io.cpu_side_io.readdata := readdata_mask_sext
      }
      .otherwise { // it's a store
        // apply the mask then write the whole cache block back, then wait for the store response
        dmemBridgeState := DMemBridgeState.WaitingForStore

        val writedata = Wire(UInt(64.W))

        // When not writing a whole double-word
        when (receivedRequest.maskmode =/= 3.U) {
          // Read in the existing piece of data at the address, so we "overwrite" only part of it
          val offset = receivedRequest.address(2, 0)
          val readdata = Wire(UInt(64.W))
          val writedata_mask = Wire(UInt(64.W))
          val writedata_mask_shifted = Wire(UInt(64.W))
          val writedata_shifted = Wire(UInt(64.W))
          val readdata_mask = Wire(UInt(64.W)) // readdata doesn't need to be shifted

          readdata := io.mem_side_io.response.bits.data

          when (receivedRequest.maskmode === 0.U) { // Byte
            writedata_mask := Cat(Fill(56, 0.U(1.W)), Fill(8, 1.U(1.W)))
          } .elsewhen (receivedRequest.maskmode === 1.U) { // Half-word
            writedata_mask := Cat(Fill(48, 0.U(1.W)), Fill(16, 1.U(1.W)))
          } .elsewhen (receivedRequest.maskmode === 2.U) { // Word
            writedata_mask := Cat(Fill(32, 0.U(1.W)), Fill(32, 1.U(1.W)))
          } .otherwise { // Double-word
            writedata_mask := Fill(64, 1.U(1.W))
          }

          writedata_mask_shifted := writedata_mask << (offset * 8.U)
          writedata_shifted := receivedRequest.writedata << (offset * 8.U)

          // The read bits and the write bits locations are mutually exclusive
          readdata_mask := ~writedata_mask_shifted
          writedata := (readdata & readdata_mask) | (writedata_shifted & writedata_mask_shifted)
        } .otherwise {
          // Write the entire double-word
          writedata := receivedRequest.writedata
        }

        //io.mem_side_io.request.valid := true.B
        //io.mem_side_io.request.bits.address := receivedRequest.address
        //io.mem_side_io.request.bits.operation := MemoryOperation.Write
        //io.mem_side_io.request.bits.writedata := writedata
        //io.mem_side_io.request.bits.dirty := 0.U // leave that for the cache to decide
        //io.mem_side_io.request.bits.age := 0.U
        sendingRequest.address :=  (receivedRequest.address >> 3.U) << 3.U
        sendingRequest.operation := MemoryOperation.Write
        sendingRequest.writedata := writedata
        sendingRequest.dirty := 0.U // leave that for the cache to decide
        sendingRequest.age := 0.U
      }
    }
  }
  .elsewhen (dmemBridgeState === DMemBridgeState.WaitingForStore) { // dmemBridgeState === DMemBridgeState.WaitingForStore
    //io.cpu_side_io.ready := false.B
    io.mem_side_io.request.valid := true.B
    io.mem_side_io.request.bits := sendingRequest
    io.cpu_side_io.good := io.mem_side_io.response.valid
    when (io.mem_side_io.response.valid) {
      dmemBridgeState := DMemBridgeState.Idle
    }
  }
  .otherwise {
    // should not be here
  }
}
