import Parser.ParseError
import SymbolTableCreator.RedefinitionError
import TypeChecker.{TypeNotInListError, UndefinedNameError, WrongArgumentAmountError, WrongTypeError}

object ErrorFormatter {
  def format(error: CompilationError) =
    error match {
      case WrongTypeError(actualType, expectedType) =>
        s"Type error: Expected $expectedType but was $actualType"
      case TypeNotInListError(actualType, expectedTypes) =>
        s"Type error: Expected one of ${expectedTypes.mkString(", ")} but was $actualType"
      case WrongArgumentAmountError(actual, expected) =>
        s"Wrong number of arguments: Expected $expected but was $actual "
      case UndefinedNameError(name) =>
        s"Undefined name: $name "
      case RedefinitionError(name) =>
        s"Redefinition error: $name already defined"
      case ParseError(msg) =>
        s"Parse error: $msg"
    }
}
