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
    if (!isSolved) _isSolved = numbers.forall(_.isSolved)
    _isSolved
  }
}
