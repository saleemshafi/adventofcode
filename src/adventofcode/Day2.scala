package adventofcode

/**
  * Created by mshafi on 12/7/17.
  */


object Day2 {

  def diff(input: Array[Int]) = {
    val nums = input.sorted
    nums.last - nums.head
  }

  def checksum(row: String):Int =
    diff(row.split("\\s")
      .map(Integer.parseInt))

  def checksum(rows: Array[String]):Int =
    rows.map(checksum).sum

  def main(args: Array[String]) = {
    assert(checksum(Array("5 1 9 5", "7 5 3", "2 4 6 8")) == 18)

    val day2input = inputAsString("dat/day2.dat")
    var inputRows = day2input.split("\n")

    println(checksum(inputRows)) // 36174
  }
}
