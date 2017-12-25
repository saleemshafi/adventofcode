package adventofcode

import java.util

/**
  * Created by mshafi on 12/24/17.
  */
object Day17 {

  def buildBuffer(steps:Int)(iterations:Int) = {
    val list = new util.LinkedList[Int]
    list.add(0)
    var position = 0
    for (i <- 1 to iterations) {
      position = (position + steps) % list.size() + 1
      list.add(position, i)
    }
    list
  }

  def optimizedShortCircuit(steps:Int)(iterations:Int) = {
    var lastShortCircuit = 0
    var position = 0
    for (i <- 1 to iterations) {
      val size = i
      position = (position + steps) % size + 1
      if (position == 1) {
        lastShortCircuit = i
      }
    }
    lastShortCircuit
  }

  def main(args: Array[String]): Unit = {
    assert(buildBuffer(3)(9).toString.equals("[0, 9, 5, 7, 2, 4, 3, 8, 6, 1]"))
    val testBuffer = buildBuffer(3)(2017)
    assert(testBuffer.get(testBuffer.indexOf(2017)+1) == 638)

    val buffer = buildBuffer(312)(2017)
    println(buffer.get(buffer.indexOf(2017)+1))  // 772

    assert(optimizedShortCircuit(3)(9) == 9)
    println(optimizedShortCircuit(312)(50000000))  // 42729050
  }
}
