package adventofcode

/**
  * Created by mshafi on 12/24/17.
  */
object Day15 {

  def generator(factor:BigInt)(iv:Int) = new Iterator[Int] {
    var prev = iv
    def next = {
      prev = ((prev * factor) % 2147483647).toInt
      prev
    }
    def hasNext = true
  }

  val MASK = Integer.MAX_VALUE >> 15
  def matching(pair:(Int, Int)) = (pair._1 & MASK) == (pair._2 & MASK)


  def judge(samples:Int)(genA:Iterator[Int], genB:Iterator[Int]) =
    genA.zip(genB).take(samples).count(matching)

  def main(args:Array[String]) = {
    val genA = generator(16807)(_)
    val genB = generator(48271)(_)
    assert(genA(65).take(5).toList == List(1092455, 1181022009, 245556042, 1744312007, 1352636452))
    assert(genB(8921).take(5).toList == List(430625591, 1233683848, 1431495498, 137874439, 285222916))
    assert(judge(5)(genA(65), genB(8921)) == 1)
    assert(judge(40000000)(genA(65), genB(8921)) == 588)

    println(judge(40000000)(genA(703), genB(516)))
  }
}
