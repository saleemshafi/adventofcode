/**
  * Created by mshafi on 12/7/17.
  */


object Day1 {
  def checksum(input: String, offset:Int = 1) = {
    val analog = input.substring(offset) + input.substring(0, offset)
    input.zip(analog)
      .map( pair => (Integer.parseInt(pair._1.toString), Integer.parseInt(pair._2.toString)))
      .map({
        case (a, b) if a == b => a
        case _ => 0
      })
      .sum
  }

  def midChecksum(input: String) = checksum(input, input.length / 2)

  def inputAsString(filename: String) = {
    val source = scala.io.Source.fromFile(filename)
    try source.mkString.trim finally source.close()
  }

  def main(args: Array[String]) = {
    assert(checksum("1122") == 3)
    assert(checksum("1111") == 4)
    assert(checksum("1234") == 0)
    assert(checksum("91212129") == 9)

    assert(midChecksum("1212") == 6)
    assert(midChecksum("1221") == 0)
    assert(midChecksum("123425") == 4)
    assert(midChecksum("123123") == 12)
    assert(midChecksum("12131415") == 4)

    val day1input = inputAsString("dat/day1.dat")
    println(checksum(day1input)) // 1341
    println(midChecksum(day1input)) // 1348
  }
}
