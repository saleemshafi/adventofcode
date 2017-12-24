package adventofcode

/**
  * Created by mshafi on 12/24/17.
  */
object Day15 {

  def generator(factor:BigInt, check:Int=>Boolean = _ => true)(iv:Int) = new Iterator[Int] {
    var prev = iv
    def next = {
      def getNext = prev = ((prev * factor) % 2147483647).toInt

      getNext
      while (!check.apply(prev)) getNext
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

    val filteredGenA = generator(16807, _ % 4 == 0)(_)
    val filteredGenB = generator(48271, _ % 8 == 0)(_)
    assert(filteredGenA(65).take(5).toList == List(1352636452, 1992081072, 530830436, 1980017072, 740335192))
    assert(filteredGenB(8921).take(5).toList == List(1233683848, 862516352, 1159784568, 1616057672, 412269392))
    assert(judge(5000000)(filteredGenA(65), filteredGenB(8921)) == 309)

    println(judge(40000000)(genA(703), genB(516)))  // 594
    println(judge(5000000)(filteredGenA(703), filteredGenB(516)))  //

  }
}
