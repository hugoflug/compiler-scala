import SymbolTableCreator.{RedefinitionError, SymbolTable}
import Compiler.CompilationError
import org.scalatest.Matchers

class SymbolTableCreatorTest extends org.scalatest.FunSuite with Matchers {

  private def createSymTable(program: String): Either[CompilationError, SymbolTable] =
    for {
      p <- Parser.parse(program)
      symTable <- SymbolTableCreator.create(p)
    } yield symTable

  test("DuplicateLocalVariableName") {
    val program =
      """class Test {
             public static void main(String[] args) {
                 int a;
                 boolean a;
             }
         }
       """

    createSymTable(program) should matchPattern { case Left(RedefinitionError("a", _)) => }
  }
}