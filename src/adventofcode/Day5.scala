package adventofcode

/**
  * Created by mshafi on 12/8/17.
  */
object Day5 {

  def increment(i:Int) = i + 1
  def limit3(i:Int) = if (i >= 3) i-1 else i+1

  def execute(offsetter:Int => Int)(instructions:Array[Int]):Int = {
    def steps(num:Int, pointer:Int):Int = {
      if (pointer >= instructions.length) {
        num
      } else {
        val offset = instructions(pointer)
        instructions(pointer) = offsetter(offset)
        steps(num+1, pointer+offset)
      }
    }

    steps(0, 0)
  }


  def main(args: Array[String]) = {

    assert(execute(increment)(Array(0, 3,  0,  1, -3)) == 5)

    val instructions = inputAsListOfStrings("dat/day5.dat").map(Integer.parseInt).array
    println(execute(increment)(instructions))  // 378980

    // reset the instructions since they were modified with the previous execution
    val instructionsAgain = inputAsListOfStrings("dat/day5.dat").map(Integer.parseInt).array
    println(execute(limit3)(instructionsAgain))  // 26889114
  }

}
