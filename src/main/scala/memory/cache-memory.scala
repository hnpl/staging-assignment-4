package dinocpu.memory

import chisel3._
import chisel3.util._

object isPowerOf2 {
  def apply(x: Int): Boolean = {
    return (x & (x-1)) == 0
  }
}

class CacheMemory(numEntries: Int, numWays: Int, latency: Int) extends Module {
  assert(isPowerOf2(numEntries))
  assert(isPowerOf2(numWays))
  val io = IO(new Bundle{
    val inst_passive_port_pair = new MemPortBusIO
    val inst_active_port_pair = Flipped(new MemPortBusIO)

    val data_passive_port_pair = new MemPortBusIO
    val data_active_port_pair = Flipped(new MemPortBusIO)
  })

  // [ Tag | Index | Offset ]
  //   ...    ...    2 : 0
  // Cache block size is 8 bytes, so we'll use 3 bits for offset.
  // numIndexBits = log2(numEntries / numWays)
  val numOffsetBits = 3
  val numIndexBits = log2Ceil(numEntries / numWays)
  val numTagBits = 64 - numIndexBits - 3
  val numEntryIndexingBits = log2Ceil(numEntries)
  def getMask(numBits: UInt) : UInt = {
    return (1.U(64.W) << numBits) - 1.U
  }

  def parseAddress(address: UInt): (UInt, UInt, UInt) = { // returning (offset, index, tag)
    return (
      address(numOffsetBits-1, 0),
      address(numIndexBits+numOffsetBits-1, numOffsetBits),
      address(63, numIndexBits+numOffsetBits)
    )
  }

  dontTouch(io)
  val (cycleCount, _) = Counter(true.B, 1 << 30) // used for LRU cache replacement policy

  def wireToInstBridge(inst_bridge: BaseIMemBridge): Unit = {
    // wire the CacheMemory passive ports to the bridge's active ports
    this.io.inst_passive_port_pair.request  <> inst_bridge.io.mem_side_io.request
    this.io.inst_passive_port_pair.response <> inst_bridge.io.mem_side_io.response
  }

  def wireToDataBridge(data_bridge: BaseDMemBridge): Unit = {
    // wire the CacheMemory passive ports to the bridge's active ports
    this.io.data_passive_port_pair.request  <> data_bridge.io.mem_side_io.request
    this.io.data_passive_port_pair.response <> data_bridge.io.mem_side_io.response
  }

  // e.g.: this is L1 cache, we are wiring it to L2 cache
  // i.e., we are wiring the active port pair of this to the passive port pair of the other
  def wireInstPortsToLowerLevelMemory(other_inst_passive_port_pair: MemPortBusIO): Unit = {
    this.io.inst_active_port_pair <> other_inst_passive_port_pair
  }

  def wireDataPortsToLowerLevelMemory(other_data_passive_port_pair: MemPortBusIO): Unit = {
    this.io.data_active_port_pair <> other_data_passive_port_pair
  }

  def wireInstPortsToHigherLevelMemory(other_inst_active_port_pair: MemPortBusIO): Unit = {
    this.io.inst_passive_port_pair <> other_inst_active_port_pair
  }

  def wireDataPortsToHigherLevelMemory(other_data_active_port_pair: MemPortBusIO): Unit = {
    this.io.data_passive_port_pair <> other_data_active_port_pair
  }

  def notUsingInstCache(): Unit = {
    this.io.inst_active_port_pair <> DontCare
    this.io.inst_passive_port_pair <> DontCare
  }

  def notUsingDataCache(): Unit = {
    this.io.data_active_port_pair <> DontCare
    this.io.data_passive_port_pair <> DontCare
  }

  def wireMemPipe(portio: MemPortBusIO, pipe: Pipe[Request]): Unit = {
    pipe.io.enq.bits      <> DontCare
    pipe.io.enq.valid     := false.B
    portio.response.valid := false.B

    // Memory is technically always ready, but we want to use the
    // ready/valid interface so that if needed we can restrict
    // executing memory operations
    portio.request.ready := true.B
  }

  val instCacheState = RegInit(ReadOnlyMemoryComponentState.Ready)
  val dataCacheState = RegInit(ReadWriteMemoryComponentState.Ready)
  val instCacheTable = Module(new CacheTable(numEntries, numWays, false))
  val dataCacheTable = Module(new CacheTable(numEntries, numWays, true))
  val instCacheMemory = Mem(numEntries, UInt(64.W))
  val dataCacheMemory = Mem(numEntries, UInt(64.W))

  val instPipe = Module(new Pipe(new Request, latency))
  val dataPipe = Module(new Pipe(new Request, latency))

  // Wire init at the beginning of each cycle
  // - Set input ports to Dontcare
  // - Set output ports to 0's
  // - All ports that send response or send request are not valid by default. Also, they are not valid/ready by default.
  this.io.inst_passive_port_pair.request <> DontCare
  this.io.inst_passive_port_pair.response <> 0.U.asTypeOf(Valid(new Response))
  this.io.inst_active_port_pair.request <> 0.U.asTypeOf(Decoupled (new Request))
  this.io.inst_active_port_pair.response <> DontCare
  this.io.data_passive_port_pair.request <> DontCare
  this.io.data_passive_port_pair.response <> 0.U.asTypeOf(Valid(new Response))
  this.io.data_active_port_pair.request <> 0.U.asTypeOf(Decoupled (new Request))
  this.io.data_active_port_pair.response <> DontCare
  this.instCacheTable.io.valid := false.B
  this.instCacheTable.io.address <> DontCare
  this.instCacheTable.io.query_mode <> DontCare
  this.instCacheTable.io.dirty_bit := false.B
  this.instCacheTable.io.age := 0.U
  this.dataCacheTable.io.valid := false.B
  this.dataCacheTable.io.address <> DontCare
  this.dataCacheTable.io.query_mode <> DontCare
  this.dataCacheTable.io.dirty_bit := false.B
  this.dataCacheTable.io.age := 0.U

  wireMemPipe(this.io.inst_passive_port_pair, instPipe)
  wireMemPipe(this.io.data_passive_port_pair, dataPipe)

  val instResponseDataReg = RegInit(UInt(64.W), 0.U)
  val instAddrReg = RegInit(UInt(64.W), 0.U)
  val dataResponseReg = RegInit(0.U.asTypeOf(new Response))
  val dataRequestReg = RegInit(0.U.asTypeOf(new Request))
  //val dataEvictAddrReg = RegInit(UInt(64.W), 0.U)
  //val dataEvictEntryReg = RegInit(0.U.asTypeOf(new CacheEntry(log2Ceil(numEntries), numTagBits)))
  val dataEvictRequestReg = RegInit(0.U.asTypeOf(new Request))

  val stats_icache_accesses = RegInit(UInt(64.W), 0.U)
  val stats_icache_hits = RegInit(UInt(64.W), 0.U)
  val stats_dcache_accesses = RegInit(UInt(64.W), 0.U)
  val stats_dcache_hits = RegInit(UInt(64.W), 0.U)

  dontTouch(stats_icache_accesses)
  dontTouch(stats_icache_hits)
  dontTouch(stats_dcache_accesses)
  dontTouch(stats_dcache_hits)

  // Logic
  if (latency == 0) { // circuit for zero latency
    this.instCacheState := ReadOnlyMemoryComponentState.Ready
    this.dataCacheState := ReadWriteMemoryComponentState.Ready

    // Since there's no latency for memory access, we forward everything from the lower level memory to higher levels
    this.io.inst_passive_port_pair <> this.io.inst_active_port_pair
    this.io.data_passive_port_pair <> this.io.data_active_port_pair
  }
  else { // non-zero read latency, zero write latency
    // ------------------------------------------------------------------------
    // Inst cache
    // ------------------------------------------------------------------------

    //printf(p"----- Cycle: ${cycleCount(10, 0)}\n")
    //when (this.instCacheState === ReadOnlyMemoryComponentState.Ready) {
    //  printf(p"InstCache: Ready\n")
    //}
    //.elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.BusyReading) {
    //  printf(p"InstCache: BusyReading\n")
    //}
    //.elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.BusyRetrying) {
    //  printf(p"InstCache: BusyRetrying\n")
    //}
    //.elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.BusyWriting) {
    //  printf(p"InstCache: BusyWriting\n")
    //}
    //.elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.WaitingForResponse) {
    //  printf(p"InstCache: WaitingForResponse\n")
    //}
    //.elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.HasResponse) {
    //  printf(p"InstCache: HasResponse\n")
    //}

    when (this.instCacheState === ReadOnlyMemoryComponentState.Ready) {
      this.io.inst_passive_port_pair.request.ready := true.B
      when (this.io.inst_passive_port_pair.request.valid) {
        stats_icache_accesses := stats_icache_accesses + 1.U
        this.instCacheState := ReadOnlyMemoryComponentState.BusyReading
        // send the request to the pipe to simulate the latency
        val incomingRequest = this.io.inst_passive_port_pair.request.bits
        instPipe.io.enq.bits  := incomingRequest
        instPipe.io.enq.valid := true.B // we need enq.valid only in this cycle so that the request will enter the instPipe pipeline
                                        // we need to turn enq.valid off in the subsequent cycles, because, if we keep it on, it will accept whatever signals supplied to enq.bits
        instAddrReg := this.io.inst_passive_port_pair.request.bits.address
      }
      .otherwise {
        instPipe.io.enq.valid := false.B
      }
    }
    .elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.BusyReading) {
      this.io.inst_passive_port_pair.request.ready := false.B
      when (instPipe.io.deq.valid) {
        // We should only be expecting a read from instruction memory
        assert(instPipe.io.deq.bits.operation === MemoryOperation.Read)
        val incomingRequest = instPipe.io.deq.bits
        // Check the cache
        instCacheTable.io.valid := true.B
        instCacheTable.io.address := incomingRequest.address
        instCacheTable.io.query_mode := 0.U
        instCacheTable.io.dirty_bit := false.B
        instCacheTable.io.age := 0.U
        val (_, __, addrTag) = parseAddress(incomingRequest.address)
        when (instCacheTable.io.success) { // cache hit
          stats_icache_hits := stats_icache_hits + 1.U
          this.instCacheState := ReadOnlyMemoryComponentState.HasResponse
          //this.io.inst_passive_port_pair.response.valid := true.B
          //this.io.inst_passive_port_pair.response.bits.data := instCacheMemory(instCacheTable.io.idx)
          instResponseDataReg := instCacheMemory(instCacheTable.io.idx)
          assert(instCacheTable.io.entry_tag === addrTag)
          //printf(p"Inst Cache hit: idx = ${instCacheTable.io.idx}; address: 0x${Hexadecimal(instAddrReg)}; data = 0x${Hexadecimal(instCacheMemory(instCacheTable.io.idx))}\n")
        }
        .otherwise { // cache miss
          //this.io.inst_active_port_pair.request <> this.io.inst_passive_port_pair.request
          // Send the request to the next level memory
          when (io.inst_active_port_pair.request.ready) { // send the request if the next level memory is ready for a new request
            this.instCacheState := ReadOnlyMemoryComponentState.WaitingForResponse
            this.io.inst_active_port_pair.request.valid := true.B
            this.io.inst_active_port_pair.request.bits.address := instAddrReg
            this.io.inst_active_port_pair.request.bits.operation := MemoryOperation.Read
          }
          .otherwise { // retry in the next cycles; this might occur when inst and data live in the same cache
            this.instCacheState := ReadOnlyMemoryComponentState.BusyRetrying
            this.io.inst_active_port_pair.request.valid := false.B
            this.io.inst_active_port_pair.request.bits.address := instAddrReg
            this.io.inst_active_port_pair.request.bits.operation := MemoryOperation.Read
          }
        }
      }
      .otherwise {
        // The cache is busy reading
        this.io.inst_passive_port_pair.response.valid := false.B
      }
    }
    .elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.BusyRetrying) {
      this.io.inst_passive_port_pair.request.ready := false.B
      when (io.inst_active_port_pair.request.ready) { // send the request
        this.instCacheState := ReadOnlyMemoryComponentState.WaitingForResponse
        this.io.inst_active_port_pair.request.valid := true.B
      }
      .otherwise { // keep retrying
        this.io.inst_active_port_pair.request.valid := false.B
      }
      this.io.inst_active_port_pair.request.bits.address := instAddrReg
      this.io.inst_active_port_pair.request.bits.operation := MemoryOperation.Read
    }
    .elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.WaitingForResponse) {
      this.io.inst_passive_port_pair.request.ready := false.B
      when (this.io.inst_active_port_pair.response.valid) { // we got the response
        this.instCacheState := ReadOnlyMemoryComponentState.HasResponse
        // forward the response to the upper level cache (without latency)
        //this.io.inst_active_port_pair.response <> this.io.inst_passive_port_pair.response
        instCacheTable.io.valid := true.B
        //instCacheTable.io.address := this.io.inst_active_port_pair.request.bits.address
        assert(this.io.inst_active_port_pair.response.bits.dirty === false.B)
        instCacheTable.io.dirty_bit := this.io.inst_active_port_pair.response.bits.dirty
        instCacheTable.io.age := 0.U
        instCacheTable.io.address := instAddrReg
        instCacheTable.io.query_mode := 1.U
        val requested_data = this.io.inst_active_port_pair.response.bits.data
        // update the data in cache memory
        instCacheMemory(instCacheTable.io.idx) := requested_data
        //printf(p"Inst Cache Miss: idx = ${instCacheTable.io.idx}; address: 0x${Hexadecimal(instAddrReg)}; data = 0x${Hexadecimal(requested_data)}\n")
        // update the response data register
        instResponseDataReg := requested_data
      }
      .otherwise {
        // turn off the cache table when not used
        // we will use the cache table again when we have the response
        instCacheTable.io.valid := false.B
      }
    }
    .elsewhen (this.instCacheState === ReadOnlyMemoryComponentState.HasResponse) {
      this.io.inst_passive_port_pair.request.ready := false.B
      // turn off the cache table as we have the response in the instResponseDataReg
      // we still have keep the data alive for this cycle
      instCacheTable.io.valid := false.B
      this.io.inst_passive_port_pair.response.bits.data := instResponseDataReg
      // response is only available for 1 cycle, it will be invalid in the next cycle
      this.instCacheState := ReadOnlyMemoryComponentState.Ready
      this.io.inst_passive_port_pair.response.valid := true.B
      //printf(p"Inst Load:  Addr=${Hexadecimal(instAddrReg)}, Data=${Hexadecimal(instResponseDataReg)}\n")
    }
    .otherwise {
      // err
      this.io.inst_active_port_pair.request.valid := false.B // no request
      this.io.inst_passive_port_pair.response.valid := false.B // no response
    }

    // ------------------------------------------------------------------------
    // Data cache
    // ------------------------------------------------------------------------
    //when (this.dataCacheState === ReadWriteMemoryComponentState.Ready) {
    //  printf(p"DataCache: Ready\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyReadingCache) {
    //  printf(p"DataCache: BusyReadingCache\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyWritingCache) {
    //  printf(p"DataCache: BusyWritingCache\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.WaitingForResponse) {
    //  printf(p"DataCache: WaitingForResponse\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyRetrying) {
    //  printf(p"DataCache: BusyRetrying\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.DirtyEvicting) {
    //  printf(p"DataCache: DirtyEvicting\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyRetryingEvicting) {
    //  printf(p"DataCache: BusyRetryingEvicting\n")
    //}
    //.elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.HasResponse) {
    //  printf(p"DataCache: HasResponse\n")
    //}
    when (this.dataCacheState === ReadWriteMemoryComponentState.Ready) {
      this.io.data_passive_port_pair.request.ready := true.B
      when (this.io.data_passive_port_pair.request.valid) { // got a request
        //if (numEntries == 128) {
        //  printf(p"Data cache received: ${Hexadecimal(this.io.data_passive_port_pair.request.bits.address)} ")
        //  when (this.io.data_passive_port_pair.request.bits.operation === MemoryOperation.Read) {
        //    printf(" (read)")
        //  } .otherwise {
        //    printf("(write) \n")
        //  }
        //}
        stats_dcache_accesses := stats_dcache_accesses + 1.U
        this.dataCacheState := ReadWriteMemoryComponentState.BusyReadingCache
        // send the request to the pipe to simulate the latency
        val incomingRequest = this.io.data_passive_port_pair.request.bits
        dataPipe.io.enq.bits  := incomingRequest
        dataPipe.io.enq.valid := true.B
        dataRequestReg := incomingRequest // store the request for later use; technically, the pipe should has this functionality ...
      }
      .otherwise {
        dataPipe.io.enq.valid := false.B
      }
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyReadingCache) {
      this.io.data_passive_port_pair.request.ready := false.B
      when (dataPipe.io.deq.valid) {
        when (dataPipe.io.deq.bits.operation === MemoryOperation.Read) {
          dataCacheTable.io.valid := true.B
          dataCacheTable.io.address := dataRequestReg.address
          dataCacheTable.io.query_mode := 0.U
          dataCacheTable.io.dirty_bit := false.B
          dataCacheTable.io.age := 0.U
          when (dataCacheTable.io.success) { // cache hit, response will be available in the next cycle
            //if (numEntries == 128) {
            //  printf("-> hit \n")
            //}
            stats_dcache_hits := stats_dcache_hits + 1.U
            this.dataCacheState := ReadWriteMemoryComponentState.HasResponse
            this.dataResponseReg.data := this.dataCacheMemory(this.dataCacheTable.io.idx)
            this.dataResponseReg.dirty := this.dataCacheTable.io.entry_dirty_bit
            this.dataResponseReg.age := this.dataCacheTable.io.entry_age
          }
          .otherwise { // cache miss
            //if (numEntries == 128) {
            //  printf("-> miss \n")
            //  //printf(p"${dataCacheTable.io}\n")
            //}
            when (this.io.data_active_port_pair.request.ready) {
              this.dataCacheState := ReadWriteMemoryComponentState.WaitingForResponse
              // send the request to the next level memory port
              io.data_active_port_pair.request.valid := true.B
              io.data_active_port_pair.request.bits := dataRequestReg
            }
            .otherwise {
              this.dataCacheState := ReadWriteMemoryComponentState.BusyRetrying
              io.data_active_port_pair.request.valid := false.B
            }
          }
        }
        .elsewhen (dataPipe.io.deq.bits.operation === MemoryOperation.Write) {
          // this is specific to the bridge in dinocpu: a store is always load first, then store
          // so a store is always a hit
          stats_dcache_hits := stats_dcache_hits + 1.U
          // transition to the updating cache state anyway
          this.dataCacheState := ReadWriteMemoryComponentState.BusyWritingCache
          dataCacheTable.io.valid := true.B
          dataCacheTable.io.address := dataRequestReg.address
          dataCacheTable.io.query_mode := 2.U
          dataCacheTable.io.dirty_bit := true.B
          dataCacheTable.io.age := dataRequestReg.age
          when (dataCacheTable.io.success === false.B) { // this means this is a write request of an eviction from the higher level cache
                                                         // and this entry couldn't replace any entry in this cache level
                                                         // in this case, we forward this eviction request to the next level
            when (this.io.data_active_port_pair.request.ready) {
              this.io.data_passive_port_pair.response.valid := true.B // every transition to the Ready state must have the valid signal on
              //this.dataCacheState := ReadWriteMemoryComponentState.BusyWritingCache
              //this.dataCacheState := ReadWriteMemoryComponentState.DirtyEvicting
              this.dataCacheState := ReadWriteMemoryComponentState.Ready // Optimization: bypassing the BusyWritingCache state and the DirtyEvicting state
              // forward the request to the lower cache, the original request is in dataRequestReg
              io.data_active_port_pair.request.valid := true.B
              io.data_active_port_pair.request.bits := dataRequestReg
            }
            .otherwise {
              this.dataCacheState := ReadWriteMemoryComponentState.BusyRetryingEvicting // Optimization: bypassing the BusyWritingCache state
              // forward the request to the lower cache
              dataEvictRequestReg := dataRequestReg
              // mark that the request is invalid in this cycle
              io.data_active_port_pair.request.valid := false.B
            }
          }
          .otherwise { // we need to update the cache memory
                       // and send an eviction request to the lower level cache if there is one
            // update the cache memory first
            dataCacheMemory(dataCacheTable.io.idx) := dataRequestReg.writedata
            // crafting an eviction request if there is one
            val thisRequestIsEvictionRequest = (dataRequestReg.age === 0.U)
            val (_, requestAddressIndex, __) = parseAddress(dataRequestReg.address)
            when (dataCacheTable.io.has_dirty_eviction) {
              val evictedEntryAddress = Cat(dataCacheTable.io.entry_tag, requestAddressIndex, 0.U(3.W)) // (tag, index, offset)
              val evictedEntryAge = dataCacheTable.io.entry_age
              val evictedEntryWriteData = dataCacheMemory(dataCacheTable.io.idx)
              dataEvictRequestReg.address := evictedEntryAddress
              dataEvictRequestReg.writedata := evictedEntryWriteData
              dataEvictRequestReg.operation := MemoryOperation.Write
              dataEvictRequestReg.dirty := true.B
              dataEvictRequestReg.age := evictedEntryAge // this is important to set age =/= 0 as we use age =/= 0 as the only condition signifying the request is an eviction request
              when (io.data_active_port_pair.request.ready) { // the lower level cache is ready to receive a request
                this.dataCacheState := ReadWriteMemoryComponentState.DirtyEvicting
                io.data_active_port_pair.request.valid := true.B
                io.data_active_port_pair.request.bits.address := evictedEntryAddress
                io.data_active_port_pair.request.bits.writedata := evictedEntryWriteData
                io.data_active_port_pair.request.bits.operation := MemoryOperation.Write
                io.data_active_port_pair.request.bits.dirty := true.B
                io.data_active_port_pair.request.bits.age := evictedEntryAge
              }
              .otherwise {
                this.dataCacheState := ReadWriteMemoryComponentState.BusyRetryingEvicting
                io.data_active_port_pair.request.valid := false.B
                // we already have the request crafted in dataEvictRequestReg
              }
            }
            .otherwise {
              this.io.data_passive_port_pair.response.valid := true.B
              this.dataCacheState := ReadWriteMemoryComponentState.Ready
            }
          }
        }
        .otherwise {
          // Should not have ReadWrite operation
          assert(dataPipe.io.deq.bits.operation =/= MemoryOperation.ReadWrite)
        }
      }
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyWritingCache) {
      this.io.data_passive_port_pair.request.ready := false.B
      // since we optimize this state out of a write request, whatever reaches here should be of a read request
      assert(dataRequestReg.operation === MemoryOperation.Read)
      dataCacheTable.io.valid := true.B
      dataCacheTable.io.address := dataRequestReg.address
      dataCacheTable.io.query_mode := 1.U
      dataCacheTable.io.dirty_bit := dataResponseReg.dirty
      dataCacheTable.io.age := dataResponseReg.age
      dataCacheMemory(dataCacheTable.io.idx) := dataResponseReg.data // must update cache
      val (_, index, __) = this.parseAddress(dataRequestReg.address)
      when (dataCacheTable.io.has_dirty_eviction === true.B) { // have an eviction
        // crafting the request
        dataEvictRequestReg.address := Cat(dataCacheTable.io.entry_tag, index, 0.U(3.W))
        dataEvictRequestReg.writedata := dataCacheMemory(dataCacheTable.io.idx)
        dataEvictRequestReg.operation := MemoryOperation.Write
        dataEvictRequestReg.dirty := true.B
        dataEvictRequestReg.age := dataCacheTable.io.entry_age
        when (io.data_active_port_pair.request.ready) {
          // send the request then move to HasResponse state
          io.data_active_port_pair.request.valid := true.B
          io.data_active_port_pair.request.bits.address := Cat(dataCacheTable.io.entry_tag, index, 0.U(3.W))
          io.data_active_port_pair.request.bits.writedata := dataCacheMemory(dataCacheTable.io.idx)
          io.data_active_port_pair.request.bits.operation := MemoryOperation.Write
          io.data_active_port_pair.request.bits.dirty := true.B
          io.data_active_port_pair.request.bits.age := dataCacheTable.io.entry_age
          this.dataCacheState := ReadWriteMemoryComponentState.HasResponse
        }
        .otherwise { // move to the BusyRetryingEvicting state
          this.dataCacheState := ReadWriteMemoryComponentState.BusyRetryingEvicting
        }
      }
      .otherwise {
        //this.io.data_passive_port_pair.response.valid := true.B
        this.dataCacheState := ReadWriteMemoryComponentState.HasResponse
      }
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.WaitingForResponse) {
      this.io.data_passive_port_pair.request.ready := false.B
      when (io.data_active_port_pair.response.valid) {
        dataResponseReg := io.data_active_port_pair.response.bits
        this.dataCacheState := ReadWriteMemoryComponentState.BusyWritingCache
      }
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyRetrying) {
      this.io.data_passive_port_pair.request.ready := false.B
      when (io.data_active_port_pair.request.ready) {
        this.dataCacheState := ReadWriteMemoryComponentState.WaitingForResponse
        io.data_active_port_pair.request.valid := true.B
        io.data_active_port_pair.request.bits := dataRequestReg
      }
      .otherwise {
        io.data_active_port_pair.request.valid := false.B
      }
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.DirtyEvicting) {
      this.io.data_passive_port_pair.request.ready := false.B
      // keep the request alive for an additional cycle (this seems to be unnecessary)
      dataCacheTable.io.valid := false.B
      io.data_active_port_pair.request.bits := dataEvictRequestReg
      this.dataCacheState := ReadWriteMemoryComponentState.Ready
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.BusyRetryingEvicting) {
      this.io.data_passive_port_pair.request.ready := false.B
      // When the next level memory is ready for accepting request, we send the request.
      when (io.data_active_port_pair.request.ready) {
        io.data_active_port_pair.request.valid := true.B
        io.data_active_port_pair.request.bits := dataEvictRequestReg
        //this.dataCacheState := ReadWriteMemoryComponentState.DirtyEvicting
        when (dataRequestReg.operation === MemoryOperation.Read) {
          this.dataCacheState := ReadWriteMemoryComponentState.HasResponse // Optimization: bypassing the DirtyEvicting state
        }
        .elsewhen (dataRequestReg.operation === MemoryOperation.Write) {
          this.dataCacheState := ReadWriteMemoryComponentState.Ready // Optimization: bypassing the DirtyEvicting state
        }
        .otherwise { //should not happen
          assert(dataRequestReg.operation =/= MemoryOperation.ReadWrite)
        }
      }
    }
    .elsewhen (this.dataCacheState === ReadWriteMemoryComponentState.HasResponse) {
      this.io.data_passive_port_pair.request.ready := false.B
      // turn off the cache table as we have the response in the dataResponseReg
      // we still have keep the data alive for this cycle
      dataCacheTable.io.valid := false.B
      this.io.data_passive_port_pair.response.bits := dataResponseReg
      // response is only available for 1 cycle, it will be invalid in the next cycle
      this.dataCacheState := ReadWriteMemoryComponentState.Ready
      this.io.data_passive_port_pair.response.valid := true.B
    }
    .otherwise {
      // err
    }
  }

}
