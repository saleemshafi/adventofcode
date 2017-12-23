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
    pinch(0)(list, lengths)
  }

  def pinch(rounds:Int)(list:Array[Int], lengths:Seq[Int]): Unit = {
    var index = 0
    var skip = 0
    for (i <- 1 to rounds) {
      for (len <- lengths) {
        index = pinch(list, len, index) + skip
        skip += 1
      }
    }
  }

  def lengths(key:String) = key.map(_.toInt) ++ Array(17, 31, 73, 47, 23)

  def sparseHash(list:Array[Int], key:String): Array[Int] = {
    pinch(64)(list, lengths(key))
    list
  }

  def denseHash(list:Array[Int], key:String) =
    for (set <- sparseHash(list, key).grouped(16))
      yield set.reduceRight((a, b) => a ^ b)

  def toHex(i:Int) = i.toHexString match {
    case hex if hex.length == 1 => "0" + hex
    case hex => hex
  }

  def knotHash(key:String): String =
    denseHash((0 to 255).toArray, key).map(toHex).mkString("")

  def main(args:Array[String]): Unit = {
    val testList = (0 to 4).toArray
    val testLengths = Array(3, 4, 1, 5)
    pinch(1)(testList, testLengths)
    assert(testList(0) == 3)
    assert(testList(1) == 4)

    assert(knotHash("") == "a2582a3a0e66e6e86e3812dcb672a272")
    assert(knotHash("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd")
    assert(knotHash("1,2,3") == "3efbe78a8d82f29979031a4aa0b16a9d")
    assert(knotHash("1,2,4") == "63960835bcdc130f0b66d7ff4f6a5a8e")

    val list = (0 to 255).toArray
    val key = Array(106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36)

    pinch(1)(list, key)
    println(list(0) * list(1))  // 11413

    println(knotHash("106,16,254,226,55,2,1,166,177,247,93,0,255,228,60,36"))  // 7adfd64c2a03a4968cf708d1b7fd418d
  }
}
