package adventofcode

/**
  * Created by mshafi on 12/14/17.
  */
object Day13 {

  case class Layer(depth:Int, range:Int) {
    def value = depth * range
    def mod = (range-1) * 2
    def captures = depth % mod == 0
  }

  def getLayers(input:Array[String]) =
    input.map(_.split(": ")).map(info => Layer(info(0).trim.toInt, info(1).trim.toInt))

  def sumCaptured(layers:Array[Layer]) =
    layers.filter(_.captures).map(_.value).sum

  def main(args:Array[String]): Unit = {

    val testLayers = getLayers(splitLines("""0: 3
                                  |1: 2
                                  |4: 4
                                  |6: 4
                                  |""".stripMargin))
    assert(sumCaptured(testLayers) == 24)

    val layers = getLayers(inputAsListOfStrings("dat/day13.dat"))
    println(sumCaptured(layers))   // 1900
  }
}
