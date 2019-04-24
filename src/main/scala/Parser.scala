object Parser {
  import fastparse._, JavaWhitespace._

  case class ParseError(msg: String, override val index: Int) extends CompilationError(index)

  def stmt[_: P]: P[Stmt] = P(assign | arrayAssign | block | syso | while_ | ifStmt)

  def ifStmt[_: P] = P(Index ~ "if" ~/ "(" ~ expr ~ ")" ~ stmt ~ ("else" ~ stmt).?)
    .map({
      case(index, cond, then_, None) => IfWithoutElse(cond, then_, index)
      case(index, cond, then_, Some(else_)) => If(cond, then_, else_, index)
    })

  def assign[_: P] = P(Index ~ id ~ "=" ~/ expr ~ ";")
    .map({ case(index, id, newVal) => Assign(id, newVal, index) })

  def arrayAssign[_: P] = P(Index ~ id ~ "[" ~ expr ~ "]" ~ "=" ~/ expr ~ ";")
    .map({ case(index, array, arrayIndex, newVal) => ArrayAssign(array, arrayIndex, newVal, index) })

  def block[_: P] = P(Index ~ "{" ~/ stmt.rep ~ "}")
    .map({ case (index, s) => Block(s, index) })

  def syso[_: P] = P(Index ~ "System.out.println" ~/ "(" ~ expr ~ ")" ~ ";")
    .map({ case (index, e) => Syso(e, index)})

  def while_[_: P] = P(Index ~ "while" ~/ "(" ~ expr ~ ")" ~ stmt)
    .map({ case(index, cond, stmt) => While(cond, stmt, index) })

  def intLit[_: P] = P(Index ~ ("0" | (CharIn("0-9") ~/ CharIn("1-9").rep)).!)
    .map({ case(index, s) => IntLit(s.toInt, index) })

  def true_[_: P] = P(Index ~ "true")
    .map(index => True(index))

  def false_[_: P] = P(Index ~ "false")
    .map(index => False(index))

  def this_[_: P] = P(Index ~ "this")
    .map(index => This(index))

  def keyword[_: P] = P("class" | "public" | "static" | "void" | "String" | "return" | "int" | "boolean" | "if" |
    "else" | "while" | "length" | "true" | "false" | "this" | "new")

  def startId[_: P] = P(CharIn("a-z") | CharIn("A-Z") | "_")
  def id[_: P] = P(Index ~ (!keyword ~ startId ~~ (startId | CharIn("0-9")).repX).!)
    .map({ case(index, s) => Identifier(s, index) })

  def newArray[_: P] = P(Index ~ "new" ~ "int" ~ "[" ~ expr ~ "]")
    .map({ case(index, e) => NewArray(e, index) })

  def newObject[_: P] = P(Index ~ "new" ~ id ~ "(" ~ ")")
    .map({ case(index, e) => NewObject(e, index) })

  def exprVal[_: P] = P(newArray | newObject | intLit | true_ | false_ | this_ | id)

  def expr[_: P] = or

  def or[_: P] = P(and ~ (Index ~ "||" ~/ and).rep)
    .map({ case (head, tail) => tail.foldLeft(head)((e1, ixAndExpr) => ixAndExpr match {
      case (index, e2) => Or(e1, e2, index)
    }) })

  def and[_: P] = P(equality ~ (Index ~ "&&" ~/ equality).rep)
    .map({ case (head, tail) => tail.foldLeft(head)((e1, ixAndExpr) => ixAndExpr match {
      case (index, e2) => And(e1, e2, index)
    }) })

  def equality[_: P] = P(compare ~ (Index ~ ("==" | "!=").! ~/ compare).rep)
    .map( { case (head, tail) => tail.foldLeft(head)({
      case (leftOp, (index, "==", rightOp)) => Equal(leftOp, rightOp, index)
      case (leftOp, (index, "!=", rightOp)) => NotEqual(leftOp, rightOp, index)
    })})

  def compare[_: P] = P(plusMinus ~ (Index ~ (">=" | "<=" | ">" | "<").! ~ plusMinus).rep)
    .map( { case (head, tail) => tail.foldLeft(head)({
      case (leftOp, (index, ">", rightOp)) => GreaterThan(leftOp, rightOp, index)
      case (leftOp, (index, "<", rightOp)) => LessThan(leftOp, rightOp, index)
      case (leftOp, (index, ">=", rightOp)) => GreaterOrEqualThan(leftOp, rightOp, index)
      case (leftOp, (index, "<=", rightOp)) => LessOrEqualThan(leftOp, rightOp, index)
    })})

  def plusMinus[_: P] = P(mult ~ (Index ~ CharIn("+\\-").! ~/ mult).rep)
    .map( { case (head, tail) => tail.foldLeft(head)({
      case (leftOp, (index, "+", rightOp)) => Plus(leftOp, rightOp, index)
      case (leftOp, (index, "-", rightOp)) => Minus(leftOp, rightOp, index)
    })})

  def mult[_: P] = P(exprInfo ~ (Index ~ "*" ~/ exprInfo).rep)
    .map({ case (head, tail) => tail.foldLeft(head)((e1, ixAndExpr) => ixAndExpr match {
      case (index, e2) => Mult(e1, e2, index)
    }) })

  def not[_: P]: P[Not] = P(Index ~ "!" ~/ expr)
    .map({ case (index, e) => Not(e, index) })

  def exprInfo[_: P]: P[Expr] = P((parens | not | exprVal).flatMap(e => arrayLength(e) | methodCall(e) | arrayLookup(e) | empty(e)))

  def empty[_: P](expr: Expr) = P("")
    .map(_ => expr)

  def parens[_: P]: P[Parens] = P(Index ~ "(" ~/ expr ~ ")")
    .map({ case (index, e) => Parens(e, index) })

  def arrayLength[_: P](array: Expr) = P(Index ~ "." ~ "length")
    .map(index => ArrayLength(array, index))

  def exprList[_: P] = P((expr ~ ("," ~/ expr).rep).?)
    .map({
      case Some((head, tail)) => head +: tail
      case None => Seq()
    })

  def methodCall[_: P](receiver: Expr) = P(Index ~ "." ~ id ~ "(" ~/ exprList ~ ")")
    .map({ case (index, methodName, args) => MethodCall(receiver, methodName, args, index) })

  def arrayLookup[_: P](array: Expr) = P(Index ~ "[" ~/ expr ~ "]")
    .map({ case (index, arrayIndex) => ArrayLookup(array, arrayIndex, index) })

  def type_[_: P]: P[TypeNode] = P(intArrayType | booleanType | intType | objectType)

  def intArrayType[_: P] = P(Index ~ "int" ~ "[]")
    .map(index => IntArrayTypeNode(index))

  def booleanType[_: P] = P(Index ~ "boolean")
    .map(index => BooleanTypeNode(index))

  def intType[_: P] = P(Index ~ "int")
    .map(index => IntTypeNode(index))

  def objectType[_: P] = P(Index ~ id)
    .map({ case (index, i) => ObjectTypeNode(i.name, index) })

  def varDecl[_: P] = P(Index ~ type_ ~ id ~ ";")
    .map({ case (index, type_, id) => VarDecl(type_, id, index) })

  def flattenFormalList(parsedInfo: (Int, TypeNode, Identifier, Seq[(Int, TypeNode, Identifier)])): Seq[Formal] = parsedInfo match {
    case (index, hType, hName, tail) =>
      Formal(hType, hName, index) +: tail.map({ case(ix, type_, name) => Formal(type_, name, ix) })
  }

  def formalList[_: P] = P(Index ~ type_ ~ id ~ ("," ~ Index ~ type_ ~ id).rep).?
    .map({
      case Some(parsedInfo) => flattenFormalList(parsedInfo)
      case None => Seq()
    })

  def methodDecl[_: P] = P(Index ~ "public" ~ type_ ~ id ~ "(" ~ formalList ~ ")" ~
    "{" ~/
      varDecl.rep ~
      stmt.rep ~
      "return" ~ expr ~ ";" ~
    "}")
    .map({
      case(index, type_, name, formals, varDecls, stmts, returnVal) =>
        MethodDecl(type_, name, formals, varDecls, stmts, returnVal, index)
    })

  def classDecl[_: P] = P(Index ~ "class" ~ id ~ "{" ~ varDecl.rep ~ methodDecl.rep ~ "}")
    .map({ case(index, name, varDecls, methodDecls) => ClassDecl(name, varDecls, methodDecls, index) })

  def mainClass[_: P] = P(Index ~ "class" ~ id ~ "{" ~
      "public" ~ "static" ~ "void" ~ "main" ~ "(" ~ "String" ~ "[" ~ "]" ~ id ~ ")" ~ "{" ~
        varDecl.rep ~
        stmt.rep ~
      "}" ~
    "}")
    .map({ case(index, name, stdArgsName, varDecls, stmts) => MainClass(name, stdArgsName, varDecls, stmts, index)})

  def program[_: P] = P(CharIn(" \n").rep ~ Index ~ mainClass ~ classDecl.rep ~ End)
    .map({ case(index, mainClass, classDecls) => Program(mainClass, classDecls, index) })

  def parse(s: String, debug: Boolean = true): Either[ParseError, Program] = {
    val result = fastparse.parse(s, program(_), verboseFailures = debug)
    result match {
      case f: Parsed.Failure =>
        val extraInfo = if (debug) ", " + f.longMsg else ""
        Left(ParseError(f.msg + extraInfo, f.index))
      case Parsed.Success(value, _) =>
        Right(value)
    }
  }
}