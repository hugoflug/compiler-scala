abstract class SyntaxTreeNode(line: Int, column: Int)
abstract class Expr(line: Int, column: Int) extends SyntaxTreeNode(line, column)
case class IntLit(value: Int, line: Int, column: Int) extends Expr(line, column)

object Parser {
  import fastparse._, NoWhitespace._

  def intLit[_: P] = P("0" | (CharIn("0-9") ~ CharIn("1-9").rep))

  def trueVal[_: P] = P("true")

  def falseVal[_: P] = P("false")

  def thisVal[_: P] = P("this")

  def exprVal[_: P] = P(intLit | trueVal | falseVal | thisVal)
}
