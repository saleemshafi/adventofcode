package adventofcode

/**
  * Created by mshafi on 12/14/17.
  */
object Day13 {

  case class Layer(depth:Int, range:Int) {
    val value = depth * range
    val mod = (range-1) * 2
    def captures(delay:Int) =
      (delay + depth) % mod == 0
  }

  def getLayers(input:Array[String]) =
    input.map(_.split(": ")).map(info => Layer(info(0).trim.toInt, info(1).trim.toInt))

  def sumCaptured(delay:Int = 0)(layers:Array[Layer]) =
    layers.filter(_.captures(delay)).map(_.value).sum

  def isCaptured(delay:Int = 0)(layers:Array[Layer]) =
    layers.find(_.captures(delay)).isDefined

  def main(args:Array[String]): Unit = {
    val testLayers = getLayers(splitLines("""0: 3
                                  |1: 2
                                  |4: 4
                                  |6: 4
                                  |""".stripMargin))
    assert(sumCaptured()(testLayers) == 24)
    assert(sumCaptured(10)(testLayers) == 0)
    assert(!isCaptured(10)(testLayers))

    assert(Stream.from(0).filter(!isCaptured(_)(testLayers)).head == 10)

    val layers = getLayers(inputAsListOfStrings("dat/day13.dat"))
    println(sumCaptured()(layers))   // 1900
    println(Stream.from(0).filter(!isCaptured(_)(layers)).head) // 3966414
  }
}
