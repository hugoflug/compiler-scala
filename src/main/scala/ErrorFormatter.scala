import Compiler.CompilationError
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
      case otherError =>
        otherError.toString
    }

  private def formatSourcePos(index: Int, program: String) = {
    val srcPos = SourcePositionFinder.find(program, index)
    s"at ${srcPos.row}:${srcPos.column}"
  }
}
