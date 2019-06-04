case class Group(numbers: List[Number]) {

  require(numbers.map(_.max).distinct.size == 1)

  val max: Int = numbers.head.max

  require(numbers.size == max)

  def stripe(): Unit = {
    numbers
      .filter(_.isSolved)
      .map(_.value.get)
      .foreach(solvedNumber => numbers.foreach(_.removePossibleNumber(solvedNumber)))
  }

  def solve1(): Unit = {
    stripe()
    numbers.foreach(_.solve1())
  }

  def solve2(): Unit = {
    this.stripe()
    this.solve1()
    val solvedNumbers = numbers.flatMap(_.value)
    val unsolvedNumbers = (1 to max).filter(!solvedNumbers.contains(_)).toList
    for (n <- unsolvedNumbers) {
      val found = numbers.filter(_.possibleNumbers.contains(n))

      if (found.size == 1) found.head.setNumber(n)
    }
  }

  private var _isSolved = false
  def isSolved: Boolean = {
    if (!_isSolved) _isSolved = numbers.forall(_.isSolved)
    _isSolved
  }

  def overlapNumbers(other: Group): List[Number] = {
    this.numbers.intersect(other.numbers)
  }
}

object Group {
  def solve3(group1: Group, group2: Group): Unit = {
    val overlap = group1.overlapNumbers(group2).filter(!_.isSolved)
    val group1Other = group1.numbers
      .filter(!_.isSolved)
      .filter(!overlap.contains(_))
    val group2Other = group2.numbers
      .filter(!_.isSolved)
      .filter(!overlap.contains(_))
    for (n: Int <- overlap.flatMap(_.possibleNumbers).toSet) {
      if (!group1Other.exists(_.possibleNumbers.contains(n))) {
        group2Other.foreach(_.removePossibleNumber(n))
        group2Other.foreach(_.solve1())
      }
      if (!group2Other.exists(_.possibleNumbers.contains(n))) {
        group1Other.foreach(_.removePossibleNumber(n))
        group1Other.foreach(_.solve1())
      }
    }
  }
}
