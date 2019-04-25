import ErrorFormatter.SourcePosition
import org.scalatest.Matchers

class ErrorFormatterTest extends org.scalatest.FunSuite with Matchers {
  test("sourcePos") {
    val program =
      """foo
        |b
        |arbar
      """.stripMargin

    ErrorFormatter.sourcePos(program, 9) shouldBe SourcePosition(3, 4)
  }
}
