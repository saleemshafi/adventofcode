package adventofcode

/**
  * Created by mshafi on 12/14/17.
  */
object Day11 {

  class OptimizedPath {
    var Ns, Ss, NEs, NWs, SEs, SWs = 0

    def numSteps = Ns + Ss + NEs + NWs + SEs + SWs

    def takeStep(step:String) = {
      step match {
        case "n" => goNorth
        case "s" => goSouth
        case "ne" => goNorthEast
        case "nw" => goNorthWest
        case "se" => goSouthEast
        case "sw" => goSouthWest
      }
      numSteps
    }

    def goNorth = {
      if (Ss > 0) { Ss -= 1 }
      else if (SEs > 0) { SEs -= 1; NEs += 1; }
      else if (SWs > 0) { SWs -= 1; NWs += 1; }
      else { Ns += 1 }
    }

    def goSouth = {
      if (Ns > 0) { Ns -= 1 }
      else if (NEs > 0) { NEs -= 1; SEs += 1; }
      else if (NWs > 0) { NWs -= 1; SWs += 1; }
      else { Ss += 1 }
    }

    def goNorthWest = {
      if (SEs > 0) { SEs -= 1 }
      else if (Ss > 0) { Ss -= 1; SWs += 1; }
      else if (NEs > 0) { NEs -= 1; Ns += 1; }
      else { NWs += 1 }
    }

    def goNorthEast = {
      if (SWs > 0) { SWs -= 1 }
      else if (Ss > 0) { Ss -= 1; SEs += 1; }
      else if (NWs > 0) { NWs -= 1; Ns += 1; }
      else { NEs += 1 }
    }

    def goSouthWest = {
      if (NEs > 0) { NEs -= 1 }
      else if (Ns > 0) { Ns -= 1; NWs += 1; }
      else if (SEs > 0) { SEs -= 1; Ss += 1; }
      else { SWs += 1 }
    }

    def goSouthEast = {
      if (NWs > 0) { NWs -= 1 }
      else if (Ns > 0) { Ns -= 1; NEs += 1; }
      else if (SWs > 0) { SWs -= 1; Ss += 1; }
      else { SEs += 1 }
    }

  }

  def numSteps(data:String) = {
    val steps = data.split(",").map(_.trim)
    val path = new OptimizedPath
    val distances = steps.map(path.takeStep)
    (path.numSteps, distances.max)
  }

  def main(arg:Array[String]): Unit = {
    val data = inputAsString("dat/day11.dat")

    assert(numSteps("ne,ne,ne")._1 == 3)
    assert(numSteps("ne,ne,sw,sw")._1 == 0)
    assert(numSteps("ne,ne,s,s")._1 == 2)
    assert(numSteps("se,sw,se,sw,sw")._1 == 3)
    val steps = numSteps(data)
    println(steps._1)  // 670
    println(steps._2)  // 1426
  }
}
