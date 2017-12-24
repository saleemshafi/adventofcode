package adventofcode

import Day10.knotHash

/**
  * Created by mshafi on 12/23/17.
  */
object Day14 {

  def fromHex(hex:Char) = Integer.parseInt(hex.toString, 16)

  def toBitmap(n:Int) = {
    val binString = n.toBinaryString.replace('1','#').replace('0','.')
    val fill = "." * (4 - binString.length)
    fill + binString
  }

  def row(key:String) =
    knotHash(key).map(fromHex).map(toBitmap).mkString("")

  def grid(key:String) =
    (0 to 127).map(key + "-" + _).map(row)

  def numUsedSpaces(g:Seq[String]) = g.map(_.count(_ == '#')).sum

  def main(args:Array[String]) = {
    assert(numUsedSpaces(grid("flqrgnkx")) == 8108)

    println(numUsedSpaces(grid("ljoxqyyw")))
  }
}
