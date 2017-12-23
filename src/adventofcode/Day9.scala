package adventofcode

/**
  * Created by mshafi on 12/11/17.
  */
object Day9 {

  def groupScore(text:String):Int = {
    var i = 0
    var score = 0
    var level = 0
    var inGarbage = false
    while (i < text.length) {
      text.charAt(i) match {
        case '{' if !inGarbage => { level += 1; i += 1 }
        case '}' if !inGarbage => { score += level; level -= 1; i += 1 }
        case '<' if !inGarbage => { inGarbage = true; i += 1 }
        case '>' if inGarbage => { inGarbage = false; i += 1 }
        case '!' => { i += 2 }
        case _ => { i+= 1 }
      }
    }
    score
  }

  def main(args:Array[String]) {
    assert(groupScore("{}") == 1)
    assert(groupScore("{{{}}}") == 6)
    assert(groupScore("{{},{}}") == 5)
    assert(groupScore("{{{},{},{{}}}}") == 16)
    assert(groupScore("{<a>,<a>,<a>,<a>}") == 1)
    assert(groupScore("{{<ab>},{<ab>},{<ab>},{<ab>}}") == 9)
    assert(groupScore("{{<!!>},{<!!>},{<!!>},{<!!>}}") == 9)
    assert(groupScore("{{<a!>},{<a!>},{<a!>},{<ab>}}") == 3)

    println(groupScore(inputAsString("dat/day9.dat")))  // 11898
  }
}
