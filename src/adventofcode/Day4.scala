package adventofcode

/**
  * Created by mshafi on 12/8/17.
  */
object Day4 {

  def validPassphrase(phrase:String):Boolean =
   phrase.split(" ")
      .groupBy(token => token)
      .filter(entry => entry._2.size > 1)
      .isEmpty

  def main(args: Array[String]) = {

    assert(validPassphrase("aa bb cc dd ee"))
    assert(!validPassphrase("aa bb cc dd aa"))
    assert(validPassphrase("aa bb cc dd aaa"))

    val inputRows = inputAsListOfStrings("dat/day4.dat")
    println(inputRows.filter(validPassphrase).length) // 477
  }
}
