package adventofcode

import scala.collection.mutable

/**
  * Created by mshafi on 12/7/17.
  */
object Day3 {

  case class Coordinate(x:Int, y:Int) {
    def manhattanDistance = Math.abs(x) + Math.abs(y)
  }

  def coordinates(x: Int, ys: Seq[Int]):Seq[Coordinate] = for (y <- ys) yield Coordinate(x,y)
  def coordinates(xs: Seq[Int], y: Int):Seq[Coordinate] = for (x <- xs) yield Coordinate(x,y)
  def coordinates(xs: Seq[Int], ys:Seq[Int]):Seq[Coordinate]= for (x <- xs; y <- ys) yield Coordinate(x,y)

  def spiralGridPath(ring:Int = 0):Stream[Coordinate] = ring match {
    case 0 => Coordinate(0,0) #:: spiralGridPath(ring+1)
    case r => (
        coordinates(r, -r+1 to r) ++ // right side
        coordinates(-r to r-1 reverse, r) ++ // top side
        coordinates(-r, -r+1 to r-1 reverse) ++ // left side
        coordinates(-r to r, -r) // bottom side
      ).toStream #::: spiralGridPath(r+1) // next ring
  }

  def adjacentSum() = {
    def adjacentCoordinates(coord:Coordinate) = coordinates(coord.x-1 to coord.x+1, coord.y-1 to coord.y+1)

    val valueMap = mutable.Map[Coordinate,Int]().withDefault(_ => 0)
    (coord:Coordinate) => {
      val value = adjacentCoordinates(coord).map(valueMap).sum
      valueMap.put(coord, if (value == 0) 1 else value)
      valueMap.get(coord).get
    }
  }

  def main(args: Array[String]) = {
    val spiralCoords = spiralGridPath()

    assert(spiralCoords(1 - 1).manhattanDistance == 0)
    assert(spiralCoords(12 - 1).manhattanDistance == 3)
    assert(spiralCoords(23 - 1).manhattanDistance == 2)
    assert(spiralCoords(1024 - 1).manhattanDistance == 31)

    println(spiralCoords(289326 - 1).manhattanDistance)  // 419

    println(spiralCoords.map(adjacentSum()).dropWhile( _ <= 289326).head) // 295229
  }
}
