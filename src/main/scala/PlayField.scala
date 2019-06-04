import scala.math.sqrt

case class PlayField(numbers: Map[(Int, Int), Number],
                     groups: List[Group]) {
  def stripe(): Unit = {
    groups.foreach(_.stripe())
  }

  def solve1(): Unit = {
    groups.foreach(_.solve1())
  }

  def solve2(): Unit = {
    groups.foreach(_.solve2())
  }

  def solve3(): Unit = {
    for {
      (g1, idx1) <- groups.zipWithIndex
      (g2, idx2) <- groups.zipWithIndex
      if idx1 > idx2
    } Group.solve3(g1, g2)
  }

  def solvedNumbers: Int = numbers.values.count(_.isSolved)
  def solvedGroups: Int = groups.count(_.isSolved)

  def printStats(): Unit = {
    println(s"Solved numbers: $solvedNumbers, Solved groups: $solvedGroups")
  }

  def isSolved: Boolean = {
    numbers.values.forall(_.isSolved)
  }

  def solve(maxTries: Int = 20): Unit = {
    solveInternal(1, maxTries)
  }

  private def solveInternal(t: Int, maxTries: Int): Unit = {
//    printNumbers()
    printStats()
    if (isSolved) {
      println("Sudoku is solved")
    } else if (t > maxTries) {
      println("Sudoku not solved, out of tries")
    } else {
      val count = solvedNumbers
      println(s"Iteration $t")
      stripe()
      solve1()
      if (count == solvedNumbers) {
        println("Solve1 failed, solve2 now")
        solve2()
        if (count == solvedNumbers) {
          println("Solve2 failed, solve3 now")
          solve3()
          if (count == solvedNumbers) {
            println("Solve3 failed, no possible moves")
          } else {
            solveInternal(t + 1, maxTries)
          }
        } else {
          solveInternal(t + 1, maxTries)
        }
      } else {
        solveInternal(t + 1, maxTries)
      }
    }
  }

  def printNumbers(possibleNumbers: Boolean = true): Unit = {
    val max = numbers.values.head.max
    val boxWith = sqrt(max).ceil.toInt + 1
    val fieldWith = numbers.values.map(_.x).max * boxWith + 1
    val fieldHeight = numbers.values.map(_.y).max * boxWith + 1
    val field = Array.fill(fieldHeight)(Array.fill(fieldWith)(" "))

    numbers.values.foreach { number =>
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

    numbers.values.filter(_.value.isDefined)

    println("---")
    println(field.map(_.mkString).mkString("\n"))
    println("---")
  }

}

object PlayField {
  def createDefault(blockSize: Int = 3): PlayField = {
    val numberCount = blockSize * blockSize
    val numbers: Map[(Int, Int), Number] = (for (x <- 1 to numberCount; y <- 1 to numberCount) yield {
      (x, y) -> Number(x, y, numberCount)
    }).toMap
    val lines: List[Group] = (for (y <- 1 to numberCount) yield {
      Group(numbers.values.filter(_.y == y).toList)
    }).toList
    val columns: List[Group] = (for (x <- 1 to numberCount) yield {
      Group(numbers.values.filter(_.x == x).toList)
    }).toList
    val fields: List[Group] = (for (x <- 0 until blockSize; y <- 0 until blockSize) yield {
      val minX = x * blockSize + 1
      val maxX = x * blockSize + blockSize
      val minY = y * blockSize + 1
      val maxY = y * blockSize + blockSize
      Group(numbers.values.filter(f => f.x >= minX && f.x <= maxX && f.y >= minY && f.y <= maxY).toList)
    }).toList

    val groups = lines ::: columns ::: fields
    PlayField(numbers, groups)
  }
}