package adventofcode

/**
  * Created by mshafi on 12/9/17.
  */
object Day7 {

  def processStructure(line:String):(String, Set[String]) = {
    val name = line.substring(0, line.indexOf(" "))  // ignore weight
    var childNodes:Set[String] = Set.empty
    val childrenStart = line.indexOf("->")
    if (childrenStart != -1) {
      val children = line.substring(childrenStart + 3)
      childNodes = children.split(", ").toSet
    }
    (name, childNodes)
  }

  def main(args:Array[String]) = {
    val input = inputAsListOfStrings("dat/day7.dat")

    var nodes:Set[String] = Set.empty
    var childNodes:Set[String] = Set.empty

    input.map(processStructure)
      .foreach(struct => {
        nodes = nodes + struct._1
        childNodes = childNodes ++ struct._2
      })

    println((nodes -- childNodes).head)   // bpvhwhh
  }
}
