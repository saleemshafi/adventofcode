package adventofcode

import scala.collection.mutable.Set

/**
  * Created by mshafi on 12/9/17.
  */
object Day6 {

  def distribute(block:Int, memory:Array[Int]) {
    val amount = memory(block)
    memory(block) = 0
    val len = memory.length
    for (i <- 1 to amount) {
      val j = (block + i) % len
      memory(j) += 1
    }
  }

  def maxBlock(memory: Array[Int]) = memory.indexOf(memory.max)


  def findRepetition(memory:Array[Int]) = {
    val seen:Set[String] = Set.empty
    var iterations = 0
    while (!seen.contains(memory.mkString(","))) {
      seen.add(memory.mkString(","))
      distribute(maxBlock(memory), memory)
      iterations += 1
    }
    iterations
  }

  def main(args:Array[String]) = {
    assert(findRepetition(Array(0, 2, 7, 0)) == 5)

    println(findRepetition(Array(4,	1,	15,	12,	0,	9,	9,	5,	5,	8,	7,	3,	14,	5,	12,	3)))  // 6681
  }
}
