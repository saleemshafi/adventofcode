package adventofcode

/**
  * Created by mshafi on 12/7/17.
  */


object Day2 {

  def minMaxDiff(nums: Array[Int]) = nums.max - nums.min

  def evenDiv(nums: Array[Int]) =
    (for (a <- nums;
          b <- nums;
          if (a < b && b % a == 0))
      yield b / a).head

  def calculate(algo: Array[Int]=>Int)(row: String):Int =
    algo(row.split("\\s").map(Integer.parseInt))

  def calculate(algo: Array[Int]=>Int, rows: Array[String]):Int =
    rows.map(calculate(algo)).sum

  def main(args: Array[String]) = {
    assert(calculate(minMaxDiff, Array("5 1 9 5", "7 5 3", "2 4 6 8")) == 18)
    assert(calculate(evenDiv, Array("5 9 2 8", "9 4 7 3", "3 8 6 5")) == 9)

    val day2input = inputAsString("dat/day2.dat")
    var inputRows = day2input.split("\n")

    println(calculate(minMaxDiff, inputRows)) // 36174
    println(calculate(evenDiv, inputRows)) //
  }
}
