package adventofcode

/**
  * Created by mshafi on 12/8/17.
  */
object Day4 {

  def validPassphrase(hashAlgo: String => String)(phrase:String):Boolean =
   phrase.split(" ")
      .groupBy(hashAlgo)
      .filter(_._2.size > 1)
      .isEmpty

  def anagram(orig:String) = orig.sorted

  def main(args: Array[String]) = {

    assert(validPassphrase(identity)("aa bb cc dd ee"))
    assert(!validPassphrase(identity)("aa bb cc dd aa"))
    assert(validPassphrase(identity)("aa bb cc dd aaa"))

    assert(validPassphrase(anagram)("abcde fghij"))
    assert(!validPassphrase(anagram)("abcde xyz ecdab"))
    assert(validPassphrase(anagram)("a ab abc abd abf abj"))
    assert(validPassphrase(anagram)("iiii oiii ooii oooi oooo"))
    assert(!validPassphrase(anagram)("oiii ioii iioi iiio"))


    val inputRows = inputAsListOfStrings("dat/day4.dat")
    println(inputRows.filter(validPassphrase(identity)).length) // 477
    println(inputRows.filter(validPassphrase(anagram)).length) // 477
  }
}
