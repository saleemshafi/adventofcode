package adventofcode

import scala.collection.mutable.Map

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

  def key(memory: Array[Int]) = memory.mkString(",")

  def findLoop(memory:Array[Int]):(Int, Int) = {
    val seen:Map[String, Int] = Map.empty
    var iterations = 0
    while (!seen.contains(key(memory))) {
      seen.put(key(memory), iterations)
      distribute(maxBlock(memory), memory)
      iterations += 1
    }
    (seen.get(key(memory)).get, iterations)
  }

  def loopSize(memory: Array[Int]) = {
    val loop = findLoop(memory)
    loop._2 - loop._1
  }

  def main(args:Array[String]) = {
    assert(findLoop(Array(0, 2, 7, 0))._2 == 5)
    assert(loopSize(Array(0, 2, 7, 0)) == 4)

    println(findLoop(Array(4,	1,	15,	12,	0,	9,	9,	5,	5,	8,	7,	3,	14,	5,	12,	3))._2)  // 6681
    println(loopSize(Array(4,	1,	15,	12,	0,	9,	9,	5,	5,	8,	7,	3,	14,	5,	12,	3)))  // 2392
  }
}
