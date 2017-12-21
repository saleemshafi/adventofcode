package adventofcode

/**
  * Created by mshafi on 12/7/17.
  */
object Day3 {


  def ringSize(value:Int, size:Int = 1):Int =
      if (value <= size * size) {
        size
      } else {
        ringSize(value, size+2)
      }

  def manhattanDistance(value:Int):Int = {
    if (value == 1) {
      return 0;
    }
    val size = ringSize(value)
    val sideSize = size - 1
    val ringNum =  sideSize / 2

    val bottomRight = Math.pow(size, 2).toInt
    val bottomLeft = bottomRight - sideSize
    val topLeft = bottomLeft - sideSize
    val topRight = topLeft - sideSize
    value match {
      case bottom if (value >= bottomLeft) => Math.abs(value - bottomLeft - ringNum) + ringNum
      case left if (value > topLeft) => ringNum + Math.abs(topLeft - value + ringNum)
      case top if (value >= topRight) => Math.abs(topRight - value + ringNum) + ringNum
      case right => ringNum + Math.abs(value - topRight + ringNum)
    }
  }


  def main(args: Array[String]) = {
    assert(manhattanDistance(1) == 0)
    assert(manhattanDistance(12) == 3)
    assert(manhattanDistance(23) == 2)
    assert(manhattanDistance(1024) == 31)

    println(manhattanDistance(289326)) // 419
  }
}
