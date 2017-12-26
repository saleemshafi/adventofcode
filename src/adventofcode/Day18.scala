package adventofcode

/**
  * Created by mshafi on 12/24/17.
  */
object Day18 {

  val RegisterPattern = "[a-z]"
  val NumberPattern = "-?\\d+"
  val ValuePattern = RegisterPattern+"|"+NumberPattern
  val PlaySound = s"snd ($ValuePattern)".r
  val SetRegister = s"set ($RegisterPattern) ($ValuePattern)".r
  val AddRegister = s"add ($RegisterPattern) ($ValuePattern)".r
  val MultiplyRegister = s"mul ($RegisterPattern) ($ValuePattern)".r
  val ModulusRegister = s"mod ($RegisterPattern) ($ValuePattern)".r
  val RecoverSound = s"rcv ($ValuePattern)".r
  val Jump = s"jgz ($ValuePattern) ($ValuePattern)".r

  type Registers = Map[Char, BigInt]
  class Execution {
    var lastFrequency:BigInt = -1
    var lastFrequencyRecovered:BigInt = -1

    def register(value:String) = value.charAt(0)
    def valueOf(value:String)(implicit reg:Registers) = reg.getOrElse(register(value), BigInt(value))

    def execute(pointer:Int, reg:Registers)(implicit instructions:Array[String]):Unit = {
      implicit val r = reg
      val (newPointer, newRegs) = instructions(pointer) match {
        case SetRegister(x,y) => (pointer+1, reg + ((register(x), valueOf(y))))
        case AddRegister(x,y) => (pointer+1, reg + ((register(x), valueOf(x) + valueOf(y))))
        case MultiplyRegister(x,y) => (pointer+1, reg + ((register(x), valueOf(x) * valueOf(y))))
        case ModulusRegister(x,y) => (pointer+1, reg + ((register(x), valueOf(x) % valueOf(y))))
        case PlaySound(x) => {
          lastFrequency = valueOf(x)
          (pointer+1, reg)
        }
        case RecoverSound(x) => {
          if (valueOf(x) != 0) {
            lastFrequencyRecovered = lastFrequency
          }
          (pointer+1, reg)
        }
        case Jump(x,y) => if (valueOf(x) > 0) (pointer+valueOf(y).toInt, reg) else (pointer+1, reg)
      }
      if (lastFrequencyRecovered == -1) {
        execute(newPointer, newRegs)
      }
    }

    def execute(implicit instructions:Array[String]):Unit = execute(0, ('a' to 'z').map((_, BigInt(0))).toMap)
  }

  def main(args: Array[String]): Unit = {
    val testInstructions = """set a 1
                             |add a 2
                             |mul a a
                             |mod a 5
                             |snd a
                             |set a 0
                             |rcv a
                             |jgz a -1
                             |set a 1
                             |jgz a -2""".stripMargin.split("\n").map(_.trim)
    val testExecution = new Execution
    testExecution.execute(testInstructions)
    assert(testExecution.lastFrequencyRecovered == 4)

    val instructions = inputAsListOfStrings("dat/day18.dat")
    val execution = new Execution
    execution.execute(instructions)
    println(execution.lastFrequencyRecovered)  // 1187
  }
}
