package adventofcode

import scala.collection.mutable.Map

/**
  * Created by mshafi on 12/9/17.
  */
object Day7 {

  class Node(val tree:Map[String, Node], val id:String, val weight:Int, val children:Set[String] = Set.empty) {
    var totalWeight = -1

    def getTotalWeight:Int = {
      if (totalWeight == -1) {
        val childrensWeight = getChildren.toList.map(_.getTotalWeight).sum
        totalWeight = weight + childrensWeight
      }
      totalWeight
    }

    def getChildren = children.map(tree)

    tree.put(id, this)
  }

  object Node {
    val InputFormat = """^(\w+) \((\d+)\)(?: -> (.+))?$""".r

    def apply(tree:Map[String, Node])(info:String):Node = info match {
      case InputFormat(id, weight, children) if children == null => new Node(tree, id, weight.toInt)
      case InputFormat(id, weight, children) =>
        new Node(tree, id, weight.toInt, children.split(", ").toSet)
    }
  }

  def findRoot(nodes:Seq[Node]):String = {
    var nodeSet:Set[String] = Set.empty
    var childNodes:Set[String] = Set.empty

    nodes.foreach(node => {
      nodeSet = nodeSet + node.id
      childNodes = childNodes ++ node.children
    })

    (nodeSet -- childNodes).head
  }

  def findUnbalanced(root:Node):Option[(Node, Int)] = {
    val weightMap = root.getChildren.groupBy(_.getTotalWeight)
    if (weightMap.keySet.size == 0) {
      None
    } else {
      val normal = weightMap.find(entry => entry._2.size > 1).get._1
      weightMap.find(entry => entry._2.size == 1) match {
        case Some((outlier, _)) => {
          val thisNode = weightMap(outlier).head
          findUnbalanced(thisNode).orElse(Some(thisNode, normal - outlier))
        }
        case None => None
      }
    }
  }

  def main(args:Array[String]) = {
    val tree:Map[String, Node] = Map.empty
    val nodes = inputAsListOfStrings("dat/day7.dat").map(Node.apply(tree))
    val root = tree(findRoot(nodes))

    println(root.id)   // bpvhwhh

    findUnbalanced(root).foreach( {
      case (unbalanced, diff) => println(unbalanced.id+" is off by "+diff+".  It should weigh "+(unbalanced.weight+diff))
    })   // tulwp is off by -8.  It should weigh 256
  }
}
