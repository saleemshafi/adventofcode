package adventofcode

import scala.collection.mutable.{ArrayBuffer, Map}

/**
  * Created by mshafi on 12/24/17.
  */
object Day16 {

  def toLetter(i:Int) = (i + 'a'.toInt).toChar

  def initialArrangement(numDancers:Int) = (0 until numDancers).map(toLetter).mkString("")

  val Spin = "s(\\d+)".r
  val Exchange = "x(\\d+)/(\\d+)".r
  val Partner = "p(\\w)/(\\w)".r
  def move(arrangement:String, mv:String): String = mv match {
    case Spin(i) => spin(arrangement, i.toInt)
    case Exchange(i,j) => exchange(arrangement, i.toInt, j.toInt)
    case Partner(a,b) => partner(arrangement, a.charAt(0), b.charAt(0))
  }

  def spin(arrangement:String, index:Int) =
    arrangement.substring(arrangement.length - index) + arrangement.substring(0, arrangement.length - index)

  def exchange(arrangement:String, i:Int, j:Int) = {
    val arr = arrangement.toCharArray
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    arr.mkString("")
  }

  def partner(arrangement:String, a:Char, b:Char) = exchange(arrangement, arrangement.indexOf(a), arrangement.indexOf(b))


  def main(args:Array[String]): Unit = {
    assert(move("abcde", "s3") == "cdeab")

    assert(initialArrangement(5) == "abcde")
    assert(move("abcde", "s1") == "eabcd")
    assert(move("eabcd", "x3/4") == "eabdc")
    assert(move("eabdc", "pe/b") == "baedc")

    val testMoves = Array("s1", "x3/4", "pe/b")
    assert(testMoves.foldLeft(initialArrangement(5))(move) == "baedc")

    val moves = inputAsString("dat/day16.dat").split(",")
    println(moves.foldLeft(initialArrangement(16))(move))  // ceijbfoamgkdnlph
  }
}
