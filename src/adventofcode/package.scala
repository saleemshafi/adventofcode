/**
  * Created by mshafi on 12/21/17.
  */
package object adventofcode {
  def inputAsString(filename: String) = {
    val source = scala.io.Source.fromFile(filename)
    try source.mkString.trim finally source.close()
  }

}
