import Parser.ParseError
import SymbolTableCreator.RedefinitionError
import TypeChecker.{IntSizeError, MultidimArrayError, TypeNotInListError, UndefinedNameError, WrongArgumentAmountError, WrongTypeError}

object ErrorFormatter {
  def format(error: CompilationError, program: String, sourceFile: String) =
    s"${formatError(error)} ${formatSourcePos(error.index, program)} in $sourceFile"

  private def formatError(error: CompilationError) =
    error match {
      case WrongTypeError(actualType, expectedType, _) =>
        s"Type error: Expected $expectedType but was $actualType"
      case TypeNotInListError(actualType, expectedTypes, _) =>
        s"Type error: Expected one of ${expectedTypes.mkString(", ")} but was $actualType"
      case WrongArgumentAmountError(actual, expected, _) =>
        s"Wrong number of arguments: Expected $expected but was $actual "
      case UndefinedNameError(name, _) =>
        s"Undefined name: $name "
      case RedefinitionError(name, _) =>
        s"Redefinition error: $name already defined"
      case ParseError(msg, _) =>
        s"Parse error: $msg"
      case MultidimArrayError(_) =>
        s"Parse error: multidimensional arrays not allowed"
      case IntSizeError(size, _) =>
        s"Number size error: $size is too large for an int"
    }

  private def formatSourcePos(index: Int, program: String) = {
    val srcPos = sourcePos(program, index)
    s"at ${srcPos.row}:${srcPos.column}"
  }

  case class SourcePosition(row: Int, column: Int)

  def sourcePos(program: String, index: Int): SourcePosition = {
    val lineLengths = program.split("\n").map(_.length).toList
    sourcePos(lineLengths, index + 1, 1)
  }

  private def sourcePos(lineLengths: List[Int], index: Int, lines: Int): SourcePosition =
    if (index <= lineLengths.head) SourcePosition(lines, index)
    else sourcePos(lineLengths.tail, index - lineLengths.head - 1, lines + 1)
}
