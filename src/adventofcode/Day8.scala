package adventofcode

import scala.collection.mutable.Map

/**
  * Created by mshafi on 12/9/17.
  */
object Day8 {
  type Registers = Map[String, Int]

  def parseChange(change:String) = {
    val pieces = change.split(" ")
    val name = pieces(0)
    val op = pieces(1)
    val amount = pieces(2)
    val amt = Integer.parseInt(amount)
    op match {
      case "dec" => (regs:Registers) => regs(name) -= amt
      case "inc" => (regs:Registers) => regs(name) += amt
      case unknownOp => throw new RuntimeException(s"Unknown operation: ${unknownOp}")
    }
  }

  def parseCondition(condition:String) = {
    val pieces = condition.split(" ")
    val name = pieces(0)
    val cond = pieces(1)
    val amount = pieces(2)
    val amt = Integer.parseInt(amount)
    cond match {
      case "==" => (regs:Registers) => regs(name) == amt
      case "!=" => (regs:Registers) => regs(name) != amt
      case ">=" => (regs:Registers) => regs(name) >= amt
      case "<=" => (regs:Registers) => regs(name) <= amt
      case ">" => (regs:Registers) => regs(name) > amt
      case "<" => (regs:Registers) => regs(name) < amt
      case unknownCond => throw new RuntimeException(s"Unknown operation: ${unknownCond}")
    }
  }

  def executeInstruction(instruction:String, registers:Map[String,Int]): Unit = {
    val pieces = instruction.trim.split(" if ")
    val change = parseChange(pieces(0))
    val condition = parseCondition(pieces(1))
    if (condition(registers)) {
      change(registers)
    }
  }

  def execute(instructions:Array[String], registers:Map[String,Int]) = {
    instructions.foreach(executeInstruction(_, registers))
    registers
  }

  def maxRegisterValue(registers:Map[String,Int]) = {
    registers.maxBy(entry => entry._2)._2
  }

  def main(args:Array[String]) = {
    val testInstructions = splitLines(
      """b inc 5 if a > 1
        |a inc 1 if b < 5
        |c dec -10 if a >= 1
        |c inc -20 if c == 10""".stripMargin)
    assert(maxRegisterValue(execute(testInstructions, Map.empty.withDefault(_ => 0))) == 1)


    val instructions = inputAsListOfStrings("dat/day8.dat")
    println(maxRegisterValue(execute(instructions, Map.empty.withDefault(_ => 0))))  // 4066
  }
}
