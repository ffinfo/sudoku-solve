import scala.language.postfixOps
import scala.math.sqrt

object Main {

  val example1: String = "" +
    "  8 69 72\n" +
    "1  75  9 \n" +
    "7 6 283 5\n" +
    " 198    4\n" +
    "2 5      \n" +
    "8  61 9  \n" +
    "3 7281 4 \n" +
    "4   36 2 \n" +
    "9825  1  \n"
  val example2: String = "" +
    "1   768 3\n" +
    " 8    6 9\n" +
    "       2 \n" +
    "  8    64\n" +
    "  7 9 5  \n" +
    "49    2  \n" +
    " 1       \n" +
    "5 2    9 \n" +
    "9 371   8\n"
  val example3: String = "" +
    "   6 4  8\n" +
    "  6   1  \n" +
    "3 4 8    \n" +
    " 517 286 \n" +
    "    6    \n" +
    " 671 849 \n" +
    "    2 9 4\n" +
    "  9   5  \n" +
    "1  4 3   \n"
  val example4: String = "" +
    "6   87 3 \n" +
    " 75  3 28\n" +
    " 3       \n" +
    "   3 2   \n" +
    "  9   8  \n" +
    "   8 1   \n" +
    "       1 \n" +
    "32 1  46 \n" +
    " 9 43   7\n"

  def main(args: Array[String]): Unit = {
    val numbers: Map[(Int, Int), Number] = (for (x <- 1 to 9; y <- 1 to 9) yield {
      (x, y) -> Number(x, y)
    }).toMap
    val lines: List[Group] = (for (y <- 1 to 9) yield {
      Group(numbers.values.filter(_.y == y).toList)
    }).toList
    val columns: List[Group] = (for (x <- 1 to 9) yield {
      Group(numbers.values.filter(_.x == x).toList)
    }).toList
    val fields: List[Group] = (for (x <- 0 until 3; y <- 0 until 3) yield {
      val minX = x * 3 + 1
      val maxX = x * 3 + 3
      val minY = y * 3 + 1
      val maxY = y * 3 + 3
      Group(numbers.values.filter(f => f.x >= minX && f.x <= maxX && f.y >= minY && f.y <= maxY).toList)
    }).toList

    val groups = lines ::: columns ::: fields

    //readFromKeyboard(numbers)
    readFromString(numbers, example4)

    groups.foreach(_.stripe())
    printNumbers(numbers.values.toList)

    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))
    groups.foreach(_.solve2())
    println(numbers.count(_._2.isSolved))

    println()
    println("Solved:")
    printNumbers(numbers.values.toList)
  }

  def printNumbers(numbers: List[Number], possibleNumbers: Boolean = true): Unit = {
    val max = numbers.head.max
    val boxWith = sqrt(max).ceil.toInt + 1
    val fieldWith = numbers.map(_.x).max * boxWith + 1
    val fieldHeight = numbers.map(_.y).max * boxWith + 1
    val field = Array.fill(fieldHeight)(Array.fill(fieldWith)(" "))

    numbers.foreach { number =>
      val xStart = number.x * boxWith - boxWith
      val yStart = number.y * boxWith - boxWith

      for (i <- 0 until boxWith) {
        field(xStart + i)(yStart) = "|"
        field(xStart)(yStart + i) = "-"
        field(xStart + i)(yStart + boxWith) = "|"
        field(xStart + boxWith)(yStart + i) = "-"
      }

      number.value match {
      case Some(v) =>
        val x = xStart + (boxWith / 2)
        val y = yStart + (boxWith / 2)
        field(y)(x) = v.toString
      case _ if possibleNumbers =>
        number.possibleNumbers.foreach { n =>
          val yPlus = (n - 1) / (boxWith - 1)
          val x = xStart + ((n - 1) % (boxWith - 1)) + 1
          val y = yStart + yPlus + 1
          field(y)(x) = n.toString
        }
      case _ =>
    }}

    numbers.filter(_.value.isDefined)

    println("---")
    println(field.map(_.mkString).mkString("\n"))
    println("---")
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
