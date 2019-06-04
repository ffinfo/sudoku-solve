import scala.language.postfixOps
import scala.math.sqrt

object Main {

  def main(args: Array[String]): Unit = {

    val playField = PlayField.createDefault(3)

    //readFromKeyboard(numbers)
    readFromString(playField.numbers, Examples.example4)

    playField.groups.foreach(_.stripe())
    playField.printNumbers(possibleNumbers = false)

    playField.solve()

    println()
    println("Solved:")
    playField.printNumbers()
  }


  def readFromKeyboard(numbers: Map[(Int, Int), Number]): Unit = {
    println("Type the sudoku:")
    val lines = for (y <- 1 to 9) yield {
      io.StdIn.readLine()
    }

    readFromString(numbers, lines.mkString("\n"))
  }

  def readFromString(numbers: Map[(Int, Int), Number], text: String): Unit = {
    val lines = text.split("\n")
    for ((line, y) <- lines.zipWithIndex) {
      require(line.length == 9, "Expecting length of 9")
      line.zipWithIndex.foreach { case (char, i) =>
        val number = numbers(i + 1, y + 1)
        char match {
          case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => number.setNumber(char.toString.toInt)
          case ' ' =>
          case _ => throw new IllegalArgumentException("Only 1-9 allowed or space for unknown")
        }
      }
    }
  }
}
