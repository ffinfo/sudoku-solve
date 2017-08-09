case class Number(x: Int, y: Int, max: Int = 9) {
  private var _possibleNumbers: List[Int] = 1 to max toList
  def possibleNumbers: List[Int] = _possibleNumbers

  private var _value: Option[Int] = None
  def value: Option[Int] = _value

  def setNumber(x: Int): Unit = {
    _possibleNumbers = Nil
    _value = Some(x)
  }
  def isSolved: Boolean = value.isDefined

  def solve1(): Unit = if (possibleNumbers.length == 1) {
    _value = Some(possibleNumbers.head)
    _possibleNumbers = Nil
  }

  def removePossibleNumber(x: Int): Unit = if (!isSolved) _possibleNumbers = possibleNumbers.filterNot(_ == x)
}
