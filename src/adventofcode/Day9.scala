package adventofcode

/**
  * Created by mshafi on 12/11/17.
  */
object Day9 {

  abstract class Visitor {
    def startGroup(level:Int):Unit
    def endGroup(level:Int):Unit
    def encounterGarbage:Unit
  }

  class ScoringVisitor extends Visitor {
    var groups = 0
    var garbage = 0
    var score = 0

    def startGroup(level:Int):Unit = {}
    def endGroup(level:Int):Unit = { groups += 1; score += level }
    def encounterGarbage:Unit = { garbage += 1}
  }

  def process[T <: Visitor](visitor: T)(text:String):T = {
    var i = 0
    var level = 0
    var inGarbage = false
    while (i < text.length) {
      text.charAt(i) match {
        case '!' => { i += 2 }
        case '{' if !inGarbage => { visitor.startGroup(level); level += 1; i += 1 }
        case '}' if !inGarbage => { visitor.endGroup(level); level -= 1; i += 1 }
        case '<' if !inGarbage => { inGarbage = true; i += 1 }
        case '>' if inGarbage => { inGarbage = false; i += 1 }
        case _ => { if (inGarbage) { visitor.encounterGarbage}; i+= 1 }
      }
    }
    visitor
  }

  def main(args:Array[String]) {
    assert(process(new ScoringVisitor)("{}").score == 1)
    assert(process(new ScoringVisitor)("{{{}}}").score == 6)
    assert(process(new ScoringVisitor)("{{},{}}").score == 5)
    assert(process(new ScoringVisitor)("{{{},{},{{}}}}").score == 16)
    assert(process(new ScoringVisitor)("{<a>,<a>,<a>,<a>}").score == 1)
    assert(process(new ScoringVisitor)("{{<ab>},{<ab>},{<ab>},{<ab>}}").score == 9)
    assert(process(new ScoringVisitor)("{{<!!>},{<!!>},{<!!>},{<!!>}}").score == 9)
    assert(process(new ScoringVisitor)("{{<a!>},{<a!>},{<a!>},{<ab>}}").score == 3)

    assert(process(new ScoringVisitor)("<>").garbage == 0)
    assert(process(new ScoringVisitor)("<random characters>").garbage == 17)
    assert(process(new ScoringVisitor)("<<<<>").garbage == 3)
    assert(process(new ScoringVisitor)("<{!>}>").garbage == 2)
    assert(process(new ScoringVisitor)("<!!>").garbage == 0)
    assert(process(new ScoringVisitor)("<!!!>>").garbage == 0)
    assert(process(new ScoringVisitor)("<{o\"i!a,<{i<a>").garbage == 10)

    var visitor = process(new ScoringVisitor)(inputAsString("dat/day9.dat"))
    println(visitor.score)  // 11898
    println(visitor.garbage)
  }
}
