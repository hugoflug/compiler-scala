object SourcePositionFinder {
  case class SourcePosition(row: Int, column: Int)

  def find(program: String, index: Int): SourcePosition = {
    val lineLengths = program.split("\n").map(_.length).toList
    find(lineLengths, index + 1, 1)
  }

  private def find(lineLengths: List[Int], index: Int, lines: Int): SourcePosition =
    if (index <= lineLengths.head) SourcePosition(lines, index)
    else find(lineLengths.tail, index - lineLengths.head - 1, lines + 1)
}
