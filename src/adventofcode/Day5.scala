package adventofcode

/**
  * Created by mshafi on 12/8/17.
  */
object Day5 {

  def execute(instructions:Array[Int]):Int = {
    def steps(num:Int, pointer:Int):Int = {
      if (pointer >= instructions.length) {
        num
      } else {
        val offset = instructions(pointer)
        instructions(pointer) = offset+1
        steps(num+1, pointer+offset)
      }
    }

    steps(0, 0)
  }


  def main(args: Array[String]) = {

    assert(execute(Array(0, 3,  0,  1, -3)) == 5)

    val instructions = inputAsListOfStrings("dat/day5.dat").map(Integer.parseInt).array
    println(execute(instructions)) // 378980
  }

}
