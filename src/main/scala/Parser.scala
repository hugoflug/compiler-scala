abstract class SyntaxTreeNode

abstract class Expr extends SyntaxTreeNode
case class IntLit(value: Int) extends Expr
case class True() extends Expr
case class False() extends Expr
case class This() extends Expr
case class Identifier(name: String) extends Expr
case class NewArray(arraySize: Expr) extends Expr
case class NewObject(typeName: Identifier) extends Expr
case class Parens(expr: Expr) extends Expr
case class ArrayLength(array: Expr) extends Expr
case class MethodCall(objectName: Expr, methodName: Identifier, argList: Seq[Expr])
case class ArrayLookup(array: Expr, index: Expr)

abstract class Type extends SyntaxTreeNode
case class BooleanType() extends Type
case class IntArrayType() extends Type
case class IntType() extends Type
case class ObjectType(name: String) extends Type

case class VarDecl(typeName: Type, id: Identifier)

object Parser {
  import fastparse._, NoWhitespace._

  def intLit[_: P] = P("0" | (CharIn("0-9") ~ CharIn("1-9").rep)).!
    .map(s => IntLit(s.toInt))

  def true_[_: P] = P("true")
    .map(_ => True())

  def false_[_: P] = P("false")
    .map(_ => False())

  def this_[_: P] = P("this")
    .map(_ => This())

  def startId[_: P] = P(CharIn("a-z") | CharIn("A-Z") | "_")
  def id[_: P] = P(startId ~ (startId | CharIn("0-9")).rep).!
    .map(s => Identifier(s))

  def newArray[_: P] = P("new int [" ~ expr ~ "]")
    .map(e => NewArray(e))

  def newObject[_: P] = P("new " ~ id ~ "()")
    .map(i => NewObject(i))

  def exprVal[_: P] = P(intLit | true_ | false_ | this_ | id)

  def expr[_: P]: P[Expr] = P(exprVal)

  def parens[_: P] = P(("(" ~ exprVal ~ ")") | exprVal)
    .map(e => Parens(e))

  def exprInfo[_: P] = ??? //P(parens ~ ())

  def arrayLength[_: P] = P(parens ~ ".length")
    .map(e => ArrayLength(e))

  //def methodCall[_: P] = P(parens ~ "." ~ id ~ "(" ~ expr.? ~ ("," ~ expr).rep )
  //  .map({ case (object_, methodName) => MethodCall(object_, methodName)})

  def arrayLookup[_: P] = ???

  def type_[_: P]: P[Type] = P(intArrayType | booleanType | intType | objectType)

  def intArrayType[_: P] = P("int []")
    .map(_ => IntArrayType())

  def booleanType[_: P] = P("boolean")
    .map(_ => BooleanType())

  def intType[_: P] = P("int")
    .map(_ => IntType())

  def objectType[_: P] = P(id)
    .map(i => ObjectType(i.name))

  def varDecl[_: P] = P(type_ ~ id ~ ";")
    .map({ case (type_, id) => VarDecl(type_, id) })

}
