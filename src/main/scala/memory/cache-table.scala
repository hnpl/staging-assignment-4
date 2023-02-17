package dinocpu.memory

import chisel3._
import chisel3.util._

class CacheTable(numEntries: Int, numWays: Int, isDCache: Boolean) extends Module {
  assert(isPowerOf2(numEntries))
  assert(isPowerOf2(numWays))
  val numOffsetBits = 3
  val numIndexBits = log2Ceil(numEntries / numWays)
  val numTagBits = 64 - numIndexBits - 3
  val numEntryIndexingBits = log2Ceil(numEntries)

  val io = IO(new Bundle{
    val address = Input(UInt(64.W)) // address of the query
    val query_mode = Input(UInt(2.W))  // query_mode == 0: check if an address exists in the cache table (i.e., read-only query)
                                       // query_mode == 1: update/allocate an entry upon a read request (i.e., read-write query for a read request)
                                       // query_mode == 2: update/allocate an entry upon a write request (i.e., read-write query for a write request)
    val dirty_bit = Input(UInt(1.W)) // this is used in query_mode == 1 when we want to set the dirty bit of that entry
                                     // this is neccessary for communication between caches, e.g. we want to bring a dirty entry from L2 cache to L1 cache
    val age = Input(UInt(32.W))      // this is used in query_mode == 2 upon an eviction
                                     // e.g., when an L1 entry is evicted to L2, the L2 cache wouldn't know the age of L1 without the age bits.
                                     // age = 0 is a special case where the cache table will decide the age of the entry.
    val valid = Input(Bool()) // valid = True if the query is valid, False otherwise

    val success = Output(Bool())    // when query_mode == 0: success == True if the address exists (cache hit), False otherwise (cache miss)
                                    // when query_mode == 1: success == True if the address is added to the cache table
                                    // when query_mode == 2: success == True if the address is added to the cache table
    val idx = Output(UInt(numEntryIndexingBits.W))  // when query_mode == 0: idx is the location of the data in cache memory if the address exists in the table; only valid if success == True
                                                    // when query_mode == 1: idx is the location of the data in cache memory of the address, also is evicted_idx; only valid if success == True
                                                    // when query_mode == 2: idx is the location of the data in cache memory of the address, also is evicted_idx; only valid if success == True
    val has_dirty_eviction = Output(Bool()) // True if there's an eviction that is dirty, False otherwise, only valid if query_mode == 1 or query_mode == 2
    val entry_tag = Output(UInt(numTagBits.W)) // this returns the tag bits of the idx before update
    val entry_dirty_bit = Output(Bool()) // this returns the dirty bit of the idx before update
    val entry_age = Output(UInt(32.W)) // this returns the age bits of the idx before update
  })

  val (cycleCount, _) = Counter(true.B, 1 << 30) // used for LRU cache replacement policy
  val table = RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new CacheEntry(log2Ceil(numEntries), numTagBits))))) // main data structure
  
  def parseAddress(address: UInt): (UInt, UInt, UInt) = { // returning (offset, index, tag)
    return (
      address(numOffsetBits-1, 0),
      address(numIndexBits+numOffsetBits-1, numOffsetBits),
      address(63, numIndexBits+numOffsetBits)
    )
  }

  io.success := false.B
  io.idx := 0.U
  io.has_dirty_eviction := false.B
  io.entry_tag := 0.U
  io.entry_dirty_bit := false.B
  io.entry_age := 0.U

  val indexMask = (1.U(numEntryIndexingBits.W) << (numIndexBits.U)) - 1.U

  // table is indexed as followed,
  //    table_index = [ way | index ]
  //                where index comes from the address' index bits

  when (io.valid) {
    val (_, index, tag) = parseAddress(io.address)

    val cases = table.zipWithIndex.map{
      case (entry, table_idx) =>
        (((table_idx.U(numEntryIndexingBits.W) & indexMask) === index) && (entry.tag === tag)) -> table_idx.U(numEntryIndexingBits.W)
    }
    //if (numEntries == 32 & isDCache) {
    //  table.zipWithIndex.foreach {
    //    case (entry, table_idx) => printf(p"idx ${(table_idx)} -> ${entry}\n")
    //  }
    //}
    val tableIdx = MuxCase(0.U(numEntryIndexingBits.W), cases)
    val cacheEntry = table(tableIdx)
    val cacheHit = cacheEntry.valid & (cacheEntry.tag === tag) & ((tableIdx & indexMask) === index)

    when (io.query_mode === 0.U) { // read-only mode
      io.success := cacheHit
      io.idx := cacheEntry.memIdx
      // update the age of the entry
      when (cacheHit) {
        table(tableIdx).age := this.cycleCount
      }
      io.entry_tag := table(tableIdx).tag
      io.entry_dirty_bit := table(tableIdx).dirty
      io.entry_age := table(tableIdx).age
    }
    .elsewhen (io.query_mode === 1.U) { // read-write mode for a read request
      // finding min age for a particular index
      // we don't have invalidations, so all entries are either valid, or being invalid because it has never been used, which means those entries have age = 0
      // so, if there is an invalid entry, MinArg is always able to assign that entry's index to the output because the age of an invalid entry is always 0
      // also, we only evict an entry if, the location of the new entry already has a dirty entry (valid == true, dirty == true)
      val minarg = Module(new MinArg(numWays))
      for (way_idx <- 0 to numWays - 1) {
        val way_idx_U = way_idx.U(numEntryIndexingBits.W)
        val mux = MuxCase(0.U(64.W),
          Range.inclusive(0, (1 << numIndexBits) - 1).map{
            case address_index => (index === address_index.U(numIndexBits.W)) -> table((way_idx_U << numIndexBits.U) + address_index.U).age
          }
        )
        minarg.io.inputs(way_idx) := mux
      }
      val evicted_way_idx = minarg.io.output
      val evicted_table_idx = Mux(cacheHit, tableIdx, Cat(evicted_way_idx, index))
      val has_eviction = (table(evicted_table_idx).valid === true.B) & (table(evicted_table_idx).dirty === true.B) & (table(evicted_table_idx).tag =/= tag)
      when (has_eviction) {
        io.has_dirty_eviction := true.B
      }
      io.entry_tag := table(evicted_table_idx).tag
      io.entry_dirty_bit := table(evicted_table_idx).dirty
      io.entry_age := table(evicted_table_idx).age
      // Update the entry
      table(evicted_table_idx).valid := true.B
      table(evicted_table_idx).dirty := io.dirty_bit
      table(evicted_table_idx).tag := tag
      table(evicted_table_idx).memIdx := evicted_table_idx
      table(evicted_table_idx).age := this.cycleCount
      io.success := true.B
      io.idx := evicted_table_idx
    }
    .elsewhen (io.query_mode === 2.U) { // read-write mode for a write request
      // finding min age for a particular index
      // we don't have invalidations, so all entries are either valid, or being invalid because it has never been used, which means those entries have age = 0
      // so, if there is an invalid entry, MinArg is always able to assign that entry's index to the output because the age of an invalid entry is always 0
      // also, we only evict an entry if, the location of the new entry already has a dirty entry (valid == true, dirty == true)
      val minarg = Module(new MinArg(numWays))
      for (way_idx <- 0 to numWays - 1) {
        val way_idx_U = way_idx.U(numEntryIndexingBits.W)
        val mux = MuxCase(0.U(64.W),
          Range.inclusive(0, (1 << numIndexBits) - 1).map{
            case address_index => (index === address_index.U(numIndexBits.W)) -> table((way_idx_U << numIndexBits.U) + address_index.U).age
          }
        )
        minarg.io.inputs(way_idx) := mux
      }
      val evicted_way_idx = minarg.io.output
      val evicted_table_idx = Mux(cacheHit, tableIdx, Cat(evicted_way_idx, index))
      when ((io.age =/= 0.U) && (io.age < table(evicted_table_idx).age)) { // this is a write request that is an eviction from a higher level cache
                                                                           // and it couldn't replace any entry in this cache level
                                                                           // we forward this to the next level and not update this cache table
        io.success := false.B
      }
      .otherwise {
        val has_eviction = (table(evicted_table_idx).valid === true.B) & (table(evicted_table_idx).dirty === true.B) & (table(evicted_table_idx).tag =/= tag)
        when (has_eviction) {
          io.has_dirty_eviction := true.B
        }
        // Update the entry
        table(evicted_table_idx).valid := true.B
        table(evicted_table_idx).dirty := true.B // it's a write request, which modifies a cache entry
        table(evicted_table_idx).tag := tag
        table(evicted_table_idx).memIdx := evicted_table_idx
        when (io.age === 0.U){
          table(evicted_table_idx).age := this.cycleCount
        }
        .otherwise {
          table(evicted_table_idx).age := io.age
        }
        io.success := true.B
        io.idx := evicted_table_idx
        io.entry_tag := table(evicted_table_idx).tag
        io.entry_dirty_bit := table(evicted_table_idx).dirty
        io.entry_age := table(evicted_table_idx).age
      }
    }
    .otherwise {
      // should not happen
    }
  }
}
