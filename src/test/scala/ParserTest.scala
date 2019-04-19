class ParserTest extends org.scalatest.FunSuite {
  import fastparse._, NoWhitespace._

  test("517 is a valid int literal") {
    assertSuccess("517", Parser.intLit(_))
  }

  test("0 is a valid int literal") {
    assertSuccess("0", Parser.intLit(_))
  }

  test("7 is a valid int literal") {
    assertSuccess("7", Parser.intLit(_))
  }

  test("02 is not a valid int literal") {
    assertFail("02", Parser.intLit(_))
  }

  private def assertSuccess(s: String, parseFn: P[_] => P[Any]) = {
    val Parsed.Success(_, index) = parse(s, parseFn)
    assert(index == s.length)
  }

  private def assertFail(s: String, parseFn: P[_] => P[Any]) = {
    parse(s, parseFn) match {
      case Parsed.Success(_, index) => assert(index != s.length)
      case Parsed.Failure(_, _, _) => assert(true)
    }
  }
}
