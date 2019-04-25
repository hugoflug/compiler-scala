import SourcePositionFinder.SourcePosition
import org.scalatest.Matchers

class SourcePositionFinderTest extends org.scalatest.FunSuite with Matchers {
  test("sourcePos") {
    val program =
      """foo
        |b
        |arbar
      """.stripMargin

    SourcePositionFinder.find(program, 9) shouldBe SourcePosition(3, 4)
  }
}
