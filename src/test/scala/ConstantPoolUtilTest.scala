import ConstantPoolUtil.{ClassRef, FieldRef, IntRef, MethodRef, Utf8Ref}
import org.scalatest.{FunSuite, Matchers}

class ConstantPoolUtilTest extends FunSuite with Matchers {
  test("constantPoolRefs") {
    val program = """
    class While {
      public static void main(String[] args) {
        int a;
        a = 5;
        while (a > 0) {
          System.out.println(a);
          a = a-1;
        }
      }
    }"""

    val classes = for {
      syntaxTree <- Parser.parse(program)
      symTable <- SymbolTableCreator.create(syntaxTree)
      _ <- TypeChecker.typeCheck(syntaxTree, symTable)
      jvmClasses = CodeGenerator.generate(syntaxTree, symTable)
    } yield jvmClasses

    classes should matchPattern { case Right(_) => }

    val refs = ConstantPoolUtil.constantPoolRefs(classes.right.get.head)
    refs shouldBe Set(ClassRef("While"), ClassRef("java/lang/Object"),
      IntRef(0), IntRef(1), IntRef(5), MethodRef("While", "main", "()V"), MethodRef("java/io/PrintStream", "println" , "(I)V"),
      FieldRef("System", "out", "Ljava/io/PrintStream;"))

    val entries = ConstantPoolUtil.constantPoolEntries(refs)
    println(entries.toList.sortBy(_.index))
  }
}
