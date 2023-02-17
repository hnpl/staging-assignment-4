package dinocpu.memory

import chisel3._
import chisel3.util._

object applyWriteMask {
  def apply(readdata: UInt, writedata:UInt, writemask: UInt): UInt = {
    val readmask = ~writemask
    (readdata & readmask) | (writedata & writemask)
  }
}

class MinArg(numInputs: Int) extends Module {
  assert(isPowerOf2(numInputs)) // numInputs must be a power of 2
  val io = IO(new Bundle{
    val inputs = Input(Vec(numInputs, UInt(64.W)))
    val output = Output(UInt(log2Ceil(numInputs).W))
  })

  val height = log2Ceil(numInputs)
  val numOutputBits = height

  if (numInputs == 1)
  {
    io.output := 0.U
  }
  else
  {
    // for 2**n elements, we need 2**n-1 comparators
    val comparison_result = Wire(Vec(numInputs-1, UInt(numOutputBits.W)))

    for (level <- height to 1 by -1) {
      val numComparisons = 1 << (level - 1)
      val startingIndex = numInputs - (1 << level)
      if (level == height) {
        for (idx <- startingIndex to (startingIndex + numComparisons - 1)) {
          val idx_U = idx.U((numOutputBits+1).W) // idx_U is well-presented using numOutputBits bits, the +1 to suppress chisel's complain
          when (io.inputs(idx * 2) < io.inputs(idx * 2 + 1)) {
            comparison_result(idx) := idx_U * 2.U
          }
          .otherwise {
            comparison_result(idx) := idx_U * 2.U + 1.U
          }
          //println(s"Level ${level} => comparison_result[${idx}] = [${idx*2}] vs [${idx*2+1}]")
        }
      }
      else {
        val currLevelOffset = startingIndex
        val prevLevelOffset = (numInputs - (1 << (level + 1)))
        for (idx <- 0 to (numComparisons-1)) {
          val idx_U = idx
          val candidate1 = comparison_result(prevLevelOffset + idx * 2)
          val candidate2 = comparison_result(prevLevelOffset + idx * 2 + 1)
          when (io.inputs(candidate1) < io.inputs(candidate2)) {
            comparison_result(currLevelOffset + idx) := candidate1
          }
          .otherwise {
            comparison_result(currLevelOffset + idx) := candidate2
          }
          //val prev = (numInputs - (1 << (level + 1)))
          //println(s"Level ${level} => comparison_result[${startingIndex + idx}] = comparison_result[${prev + idx*2}] vs comparison_result[${prev+idx*2+1}]")
        }
      }
    }
    io.output := comparison_result(numInputs-2)
  }
}