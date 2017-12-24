package adventofcode

import scala.collection.mutable.{Map, Set}
/**
  *
  * Created by mshafi on 12/14/17.
  */
object Day12 {

  def parseConnection(connection:String) = {
    val pieces = connection.split("<->")
    (pieces(0).trim.toInt, pieces(1).split(", ").map(_.trim.toInt))
  }

  def group(connections:Array[(Int,Array[Int])]): Map[Int,Set[Int]] = {
    def connect(groups:Map[Int, Set[Int]], a:Int, b:Int): Unit = {
      val groupA = groups(a)
      val groupB = groups(b)
      if (groupA.ne(groupB)) {
        for(bVal <- groupB) {
          groupA.add(bVal)
          groups(bVal) = groupA
        }
      }
    }

    val groups = Map[Int, Set[Int]]().withDefault(Set(_))
    for (conn <- connections;
         a = conn._1;
         b <- conn._2) {
      connect(groups, a, b)
      connect(groups, b, a)
    }
    groups
  }

  def main(args:Array[String]): Unit = {
    val data = inputAsListOfStrings("dat/day12.dat")

    val connections = data.map(parseConnection)
    val groups = group(connections)

    println(groups(0).size)
  }
}
