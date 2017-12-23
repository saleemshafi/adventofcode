package adventofcode

/**
  * Created by mshafi on 12/14/17.
  */
object Day10 {

  def pinch(list:Array[Int], length:Int, index:Int): Int = {
    def swap(list:Array[Int], a:Int, b:Int): Unit = {
      val temp = list(a)
      list.update(a, list(b))
      list.update(b, temp)
    }

    for (i <- 0 to (length-1) / 2) {
      val a = (index + i) % list.length
      val b = (index + (length-1) - i) % list.length
      swap(list, a, b)
    }
    (index + length) % list.length;
  }

  def pinch(list:Array[Int], lengths:Array[Int]): Unit = {
    var index = 0
    for (i <- 0 to lengths.length-1) {
      index = pinch(list, lengths(i), index) + i
    }
  }

  def main(args:Array[String]): Unit = {
    val testList = (0 to 4).toArray
    val testLengths = Array(3, 4, 1, 5)
    pinch(testList, testLengths)
    assert(testList(0) == 3)
    assert(testList(1) == 4)


    val list = (0 to 255).toArray
    val lengths = Array(106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36)

    pinch(list, lengths)

    println(list(0) * list(1))  // 11413
  }
}
