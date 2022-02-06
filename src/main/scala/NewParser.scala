import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Rfc5234.{alpha, digit}

import cats.implicits._

import cats.data.NonEmptyList

object NewParserOps {
    implicit class ParserLiteral(private val sc: StringContext) extends AnyVal {
        def p(args: Any*): P[Unit] = P.string(sc.raw())
    }

    private def ws: P[Unit] = P.charIn(" \t\n\r\f").rep.void | p"/*"

    implicit class Parser0Ops[A, B](private val parser: P0[A]) extends AnyVal {
        def **>(that: P0[B]): P0[B] = parser *> ws.? *> that
        def <**(that: P0[B]): P0[A] = parser <* ws.? <* that
        def ~~(that: P0[B]): P0[(A, B)] = (parser ~ ws.? ~ that).map { case ((a, _), b) => (a, b) }
    }

    implicit class ParserOps[A, B](private val parser: P[A]) extends AnyVal {
        def **>(that: P0[B]): P[B] = parser *> ws.? *> that
        def <**(that: P0[B]): P[A] = parser <* ws.? <* that
        def ~~(that: P0[B]): P[(A, B)] = (parser ~ ws.? ~ that).map { case ((a, _), b) => (a, b) }
    }
}

object NewParser {
    import NewParserOps._

    def index: P.With1[Int] = P.index.with1
    def startId: P[Char] = alpha | P.charIn("_")
    def idChar: P[Char] = startId | digit
    def keyword: P[Unit] = P.oneOf(List(p"class", p"static", p"void", p"return", p"int", p"boolean", p"if", p"else",
                               p"while", p"length", p"true", p"false", p"this")) <* !idChar
    def id: P[Identifier] = (!keyword).with1 *> (index ~ (startId *> idChar.rep0).string)
      .map { case(index, s) => Identifier(s, index) }
    def ws = P.charIn(" \t\n\r\f").rep | p"/*"
    def newArray = (index ~ (p"new" *> ws *> p"int" **> p"[" **> expr <** p"]"))
      .map { case(index, e) => NewArray(e, index) }
    def newObject = (index ~ (p"new" *> ws *> id <** p"(" <** p")"))
      .map { case(index, i) => NewObject(i, index) }
    def expr: P[Expr] = P.defer(or)
    def or: P[Expr] = (and ~~ ((index <* p"||") ~~ and).rep0)
      .map { case (head, tail) => tail.foldLeft(head)((e1, ixAndExpr) => ixAndExpr match {
        case (index, e2) => Or(e1, e2, index)
      }) }
    def and: P[Expr] = (equality ~~ ((index <* p"&&") ~~ equality).rep0)
      .map { case (head, tail) => tail.foldLeft(head)((e1, ixAndExpr) => ixAndExpr match {
        case (index, e2) => And(e1, e2, index)
      }) }

    def equalityOp: P[String] = (p"==" | p"!=").string
    def equality: P[Expr] = (compare ~~ (index ~ equalityOp ~~ compare).rep0)
      .map { case (head, tail) => tail.foldLeft(head)({
        case (leftOp, ((index, "=="), rightOp)) => Equal(leftOp, rightOp, index)
        case (leftOp, ((index, "!="), rightOp)) => NotEqual(leftOp, rightOp, index)
      })}

    def compareOp: P[String] = (p">=" | p"<=" | p">" | p"<").string
    def compare: P[Expr] = (plusMinus ~~ (index ~ compareOp ~~ plusMinus).rep0)
      .map { case (head, tail) => tail.foldLeft(head)({
        case (leftOp, ((index, ">"), rightOp)) => GreaterThan(leftOp, rightOp, index)
        case (leftOp, ((index, "<"), rightOp)) => LessThan(leftOp, rightOp, index)
        case (leftOp, ((index, ">="), rightOp)) => GreaterOrEqualThan(leftOp, rightOp, index)
        case (leftOp, ((index, "<="), rightOp)) => LessOrEqualThan(leftOp, rightOp, index)
      })}

    def plusMinus: P[Expr] = (mult ~~ (index ~ P.charIn("+-") ~~ mult).rep0)
      .map { case (head, tail) => tail.foldLeft(head)({
        case (leftOp, ((index, '+'), rightOp)) => Plus(leftOp, rightOp, index)
        case (leftOp, ((index, '-'), rightOp)) => Minus(leftOp, rightOp, index)
      })}

    def mult: P[Expr] = (notExpr ~~ (index ~ p"*" ~~ notExpr).rep0)
      .map { case (head, tail) => tail.foldLeft(head)((e1, ixAndExpr) => ixAndExpr match {
        case ((ix, _),  e2) => Mult(e1, e2, ix)
      }) }

    def notExpr: P[Expr] = not | exprInfo

    def not: P[Expr] = P.defer(index ~ p"!" ~~ (not | exprInfo))
      .map { case ((index, _), e) => Not(e, index) }

    def exprInfo: P[Expr] = (parens | exprVal).flatMap(exprInfoExt(_))

    def exprInfoExt(e: Expr): P0[Expr] = noExprInfoExt(e) //| arrayLength(e) | methodCall(e) | arrayLookup(e)

    def noExprInfoExt(e: Expr): P0[Expr] = (!P.charIn(".["))
      .as(e)

    def empty(expr: Expr): P0[Expr] = P.pure(expr)

    def exprVal: P[Expr] = id | newArray | newObject | intLit | true_ | false_ | this_

    def true_ : P[Expr] = (index <* p"true")
      .map { index => True(index) }

    def false_ : P[Expr] = (index <* p"false")
      .map { index => False(index) }

    def this_ : P[Expr] = (index <* p"this")
      .map { index => This(index) }

    def parens: P[Expr] = (index ~ expr.between(p"(", p")"))
      .map({ case (index, e) => Parens(e, index) })

    def arrayLength(array: Expr): P[Expr] = (index <* p"." <** p"length")
      .map { index => ArrayLength(array, index) }

    def exprList: P0[Seq[Expr]] = ((expr ~~ (p"," **> expr).rep0).?)
      .map {
        case Some((head, tail)) => head +: tail
        case None => Seq()
      }

    def methodCall(receiver: Expr): P[Expr] = (index ~ p"." ~~ id ~~ exprList.between(p"(", p")"))
      .map({ case (((index, _), methodName), args) => MethodCall(receiver, methodName, args, index) })

    def arrayLookup(array: Expr): P[Expr] = (index ~ p"[" ~~ expr <** p"]")
      .map({ case ((index, _), arrayIndex) => ArrayLookup(array, arrayIndex, index) })

    def intLit: P[Expr] = (index ~ (p"0" | (P.charIn("123456789") ~ digit.rep0)).string)
      .map { case(index, s) => IntLit(s.toLong, index) }

    def stmt: P[Stmt] = /*assign | arrayAssign | block | syso | while_ |*/ ifStmt

    def ifStmt: P[Stmt] = (index ~ p"if" ~~ expr.between(p"(", p")") ~~ stmt ~~ (p"else" ~~ stmt).?)
      .map {
        case((((index, _), cond), then_), None) => IfWithoutElse(cond, then_, index)
        case((((index, _), cond), then_), Some((_, else_))) => If(cond, then_, else_, index)
      }

    def assign: P[Stmt] = (index ~ id ~~ p"=" ~~ expr <** p";")
      .map { case(((index, id), _), newVal) => Assign(id, newVal, index) }

  // TODO: between doesn't work with thiespace
  def arrayAssign: P[Stmt] = (index ~ id ~~ expr.between(p"[", p"]") ~~ p"=" ~~ expr <** p";")
    .map { case((((index, array), arrayIndex), _), newVal) => ArrayAssign(array, arrayIndex, newVal, index) }

  def block: P[Stmt] = (index ~ stmt.rep.between(p"{", p"}"))
    .map { case (index, s) => Block(s.toList.toSeq, index) }

  def syso: P[Stmt] = (index ~ p"System.out.println" ~~ expr.between(p"(", p")") <** p";")
    .map { case ((index, _), e) => Syso(e, index) }

  def while_ : P[Stmt] = (index ~ p"while" ~~ expr.between(p"(", p")") ~~ stmt)
    .map { case (((index, _), cond), stmt) => While(cond, stmt, index) }

  def type_ : P[TypeNode] = intArrayType | booleanType | intType | objectType
    
  def intArrayType: P[TypeNode] = (index ~ p"int" <** p"[" <** p"]")
    .map { case (index, _) => IntArrayTypeNode(index) }

  def booleanType: P[TypeNode] = (index ~ p"boolean")
    .map { case (index, _) => BooleanTypeNode(index) }

  def intType: P[TypeNode] = (index ~ p"int")
    .map { case (index, _) => IntTypeNode(index) }

  def objectType: P[TypeNode] = (index ~ id)
    .map { case (index, i) => ObjectTypeNode(i.name, index) }
  
  def genVarDecl: P[(TypeNode, Identifier)] = (type_ ~ ws ~~ id).map { case ((a, b), c) => (a, c)} | (intArrayType ~~ id)

  def varDecl: P[VarDecl] = (index ~ genVarDecl <** p";")
    .map { case (index, (type_, id)) => VarDecl(type_, id, index) }

  def formalList: P0[Seq[Formal]] = (index ~ genVarDecl ~~ (p"," **> index ~ genVarDecl).rep0).?
    .map {
      case Some(parsedInfo) => flattenFormalList(parsedInfo)
      case None => Seq.empty
    }
    
  def flattenFormalList(parsedInfo: ((Int, (TypeNode, Identifier)), List[(Int, (TypeNode, Identifier))])): Seq[Formal] = parsedInfo match {
    case ((index, (hType, hName)), tail) =>
      Formal(hType, hName, index) +: tail.map({ case(ix, (type_, name)) => Formal(type_, name, ix) })
  }
/*
  def methodDecl: P[MethodDecl] = (index ~ p"public" ~ ws ~~ type_ ~ ws ~~ id ~~ formalList.between(p"(", p")") ~~
    p"{" ~~
      varDecl.rep ~~
      stmt.rep ~~
      p"return" ~~ expr ~~ p";" ~~
    p"}")
    .map {
      case(index, type_, name, formals, varDecls, stmts, returnVal) =>
        MethodDecl(type_, name, formals, varDecls, stmts, returnVal, index)
    }
    */
/*
  def classDecl[_: P] = P(Index ~ "class" ~~/ ws ~ id ~ "{" ~ varDecl.rep ~ methodDecl.rep ~ "}")
    .map({ case(index, name, varDecls, methodDecls) => ClassDecl(name, varDecls, methodDecls, index) })

  def mainClass[_: P] = P(Index ~ "class" ~~/ ws ~ id ~ "{" ~
      "public" ~~ ws ~ "static" ~~ ws ~ "void" ~~ ws ~ "main" ~ "(" ~ "String" ~ "[" ~ "]" ~ id ~ ")" ~ "{" ~
        varDecl.rep ~
        stmt.rep ~
      "}" ~
    "}")
    .map({ case(index, name, stdArgsName, varDecls, stmts) => MainClass(name, stdArgsName, varDecls, stmts, index)})

  def program[_: P] = P(ws.rep ~ Index ~ mainClass ~ classDecl.rep ~ End)
    .map({ case(index, mainClass, classDecls) => Program(mainClass, classDecls, index) })


    */
}