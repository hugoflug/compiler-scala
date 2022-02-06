import org.scalatest.Matchers

class NewParserTest extends org.scalatest.FunSuite with Matchers {

  import NewParser._

  test("Identifier") {
    id.parse("a") shouldBe Right(("", Identifier("a", 0)))
    id.parse("hello") shouldBe Right(("", Identifier("hello", 0)))
    id.parse("1hello") shouldBe 'left
    id.parse("static") shouldBe 'left
    id.parse("statico") shouldBe Right(("", Identifier("statico", 0)))
  }

  test("NewArray") {
    newArray.parse("new int[5]") shouldBe 'right
    newArray.parse("new int [5]") shouldBe 'right
    newArray.parse("new int [  5  ]") shouldBe 'right
  }

  test("or") {
    //or.parse("5 || 7 ||3") shouldBe Right("", IntLit(5, 0))
  }

  test ("IntLit") {
    intLit.parse("0") shouldBe Right("", IntLit(0, 0))
    intLit.parse("1") shouldBe Right("", IntLit(1, 0))
    intLit.parse("10") shouldBe Right("", IntLit(10, 0))
    intLit.parse("514") shouldBe Right("", IntLit(514, 0))
  }

  test ("equality") {
    equality.parse("1==1") shouldBe Right("", Equal(IntLit(1, 0), IntLit(1, 3), 1))
    equality.parse("1== 1") shouldBe Right("", Equal(IntLit(1, 0), IntLit(1, 4), 1))
    equality.parse("1 == 1") shouldBe Right("", Equal(IntLit(1, 0), IntLit(1, 5), 2))

  }
}
