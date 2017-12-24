package adventofcode

import Day10.knotHash
import scala.collection.mutable.ArrayBuffer

/**
  * Created by mshafi on 12/23/17.
  */
object Day14 {

  type Grid = ArrayBuffer[ArrayBuffer[Char]]

  def fromHex(hex:Char) = Integer.parseInt(hex.toString, 16)

  def toBitmap(n:Int) = {
    val binString = n.toBinaryString.replace('1','#').replace('0','.')
    val fill = "." * (4 - binString.length)
    fill + binString
  }

  def row(key:String) =
    new ArrayBuffer[Char]() ++ knotHash(key).map(fromHex).map(toBitmap).mkString("")

  def grid(key:String) =
    new ArrayBuffer ++ (0 to 127).map(key + "-" + _).map(row)

  def numUsedSpaces(g:Grid) = g.map(_.count(_ != '.')).sum

  def markGroups(grid:Grid):Int = {
    var groupNum = 0

    def markGroup(i: Int, j: Int): Unit =
      if (i >= 0 && i < grid.size && j >= 0 && j < grid(i).size && grid(i)(j) == '#') {
        grid(i)(j) = groupNum.toString.last
        markGroup(i-1, j)
        markGroup(i+1, j)
        markGroup(i, j-1)
        markGroup(i, j+1)
      }

    for (i <- grid.indices;
         j <- grid(i).indices if grid(i)(j) == '#') {
          groupNum += 1
          markGroup(i, j)
    }
    groupNum
  }

  def main(args:Array[String]) = {
    val testGrid = grid("flqrgnkx")

    assert(numUsedSpaces(testGrid) == 8108)
    assert(markGroups(testGrid) == 1242)

    val realGrid = grid("ljoxqyyw")
    println(numUsedSpaces(realGrid)) // 8316
    println(markGroups(realGrid))  // 1074

  }
}
