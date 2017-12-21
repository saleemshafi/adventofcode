/**
  * Created by mshafi on 12/7/17.
  */


object Day1 {
  def checksum(input: String) = {
    val full = input + input.charAt(0)
    full.sliding(2)
      .map( pair => (Integer.parseInt(pair.substring(0,1)), Integer.parseInt(pair.substring(1,2))))
      .map( _ match {
        case (a, b) if a == b => a
        case _ => 0
      })
      .sum
  }

  def inputAsString(filename: String) = {
    val source = scala.io.Source.fromFile(filename)
    try source.mkString.trim finally source.close()
  }

  def main(args: Array[String]) = {
    assert(checksum("1122") == 3)
    assert(checksum("1111") == 4)
    assert(checksum("1234") == 0)
    assert(checksum("91212129") == 9)

    println(checksum(inputAsString("dat/day1.dat"))) // 1341
  }
}
