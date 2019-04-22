object Parser {
  import fastparse._, JavaWhitespace._

  case class ParseError(msg: String) extends CompilerError(msg)

  def stmt[_: P]: P[Stmt] = P(assign | arrayAssign | block | syso | while_ | ifStmt)

  def ifStmt[_: P] = P("if" ~/ "(" ~ expr ~ ")" ~ stmt ~ ("else" ~ stmt).?)
    .map({
      case(cond, then_, None) => IfWithoutElse(cond, then_)
      case(cond, then_, Some(else_)) => If(cond, then_, else_)
    })

  def assign[_: P] = P(id ~ "=" ~/ expr ~ ";")
    .map({ case(id, newVal) => Assign(id, newVal) })

  def arrayAssign[_: P] = P(id ~ "[" ~ expr ~ "]" ~ "=" ~/ expr ~ ";")
    .map({ case(array, index, newVal) => ArrayAssign(array, index, newVal) })

  def block[_: P] = P("{" ~/ stmt.rep ~ "}")
    .map(s => Block(s))

  def syso[_: P] = P("System.out.println" ~/ "(" ~ expr ~ ")" ~ ";")
    .map(e => Syso(e))

  def while_[_: P] = P("while" ~/ "(" ~ expr ~ ")" ~ stmt)
    .map({ case(cond, stmt) => While(cond, stmt) })

  def intLit[_: P] = P("0" | (CharIn("0-9") ~/ CharIn("1-9").rep)).!
    .map(s => IntLit(s.toInt))

  def true_[_: P] = P("true")
    .map(_ => True())

  def false_[_: P] = P("false")
    .map(_ => False())

  def this_[_: P] = P("this")
    .map(_ => This())

  def startId[_: P] = P(CharIn("a-z") | CharIn("A-Z") | "_")
  def id[_: P] = P(startId ~~ (startId | CharIn("0-9")).repX).!
    .map(s => Identifier(s))

  def newArray[_: P] = P("new" ~ "int" ~ "[" ~ expr ~ "]")
    .map(e => NewArray(e))

  def newObject[_: P] = P("new" ~ id ~ "(" ~ ")")
    .map(i => NewObject(i))

  def exprVal[_: P] = P(newArray | newObject | intLit | true_ | false_ | this_ | id)

  def expr[_: P] = or

  def or[_: P] = P(and ~ ("||" ~/ and).rep)
    .map({ case (head, tail) => tail.foldLeft(head)(Or) })

  def and[_: P] = P(equality ~ ("&&" ~/ equality).rep)
    .map({ case (head, tail) => tail.foldLeft(head)(And) })

  def equality[_: P] = P(compare ~ (("==" | "!=").! ~/ compare).rep)
    .map( { case (head, tail) => tail.foldLeft(head)({
      case (leftOp, ("==", rightOp)) => Equal(leftOp, rightOp)
      case (leftOp, ("!=", rightOp)) => NotEqual(leftOp, rightOp)
    })})

  def compare[_: P] = P(plusMinus ~ ((">=" | "<=" | ">" | "<").! ~ plusMinus).rep)
    .map( { case (head, tail) => tail.foldLeft(head)({
      case (leftOp, (">", rightOp)) => GreaterThan(leftOp, rightOp)
      case (leftOp, ("<", rightOp)) => LessThan(leftOp, rightOp)
      case (leftOp, (">=", rightOp)) => GreaterOrEqualThan(leftOp, rightOp)
      case (leftOp, ("<=", rightOp)) => LessOrEqualThan(leftOp, rightOp)
    })})

  def plusMinus[_: P] = P(mult ~ (CharIn("+\\-").! ~/ mult).rep)
    .map( { case (head, tail) => tail.foldLeft(head)({
      case (leftOp, ("+", rightOp)) => Plus(leftOp, rightOp)
      case (leftOp, ("-", rightOp)) => Minus(leftOp, rightOp)
    })})

  def mult[_: P] = P(exprInfo ~ ("*" ~/ exprInfo).rep)
    .map({ case (head, tail) => tail.foldLeft(head)(Mult) })

  def not[_: P]: P[Not] = P("!" ~/ expr)
    .map(e => Not(e))

  def exprInfo[_: P]: P[Expr] = P((parens | not | exprVal).flatMap(e => arrayLength(e) | methodCall(e) | arrayLookup(e) | empty(e)))

  def empty[_: P](expr: Expr) = P("")
    .map(_ => expr)

  def parens[_: P]: P[Parens] = P("(" ~/ expr ~ ")")
    .map(e => Parens(e))

  def arrayLength[_: P](array: Expr) = P("." ~ "length")
    .map(_ => ArrayLength(array))

  def exprList[_: P] = P((expr ~ ("," ~/ expr).rep).?)
    .map({
      case Some((head, tail)) => head +: tail
      case None => Seq()
    })

  def methodCall[_: P](receiver: Expr) = P("." ~ id ~ "(" ~/ exprList ~ ")")
    .map({ case (methodName, args) => MethodCall(receiver, methodName, args) })

  def arrayLookup[_: P](array: Expr) = P("[" ~/ expr ~ "]")
    .map(index => ArrayLookup(array, index))

  def type_[_: P]: P[Type] = P(intArrayType | booleanType | intType | objectType)

  def intArrayType[_: P] = P("int" ~ "[]")
    .map(_ => IntArrayType())

  def booleanType[_: P] = P("boolean")
    .map(_ => BooleanType())

  def intType[_: P] = P("int")
    .map(_ => IntType())

  def objectType[_: P] = P(id)
    .map(i => ObjectType(i.name))

  def varDecl[_: P] = P(type_ ~ id ~ ";")
    .map({ case (type_, id) => VarDecl(type_, id) })

  def flattenFormalList(parsedInfo: (Type, Identifier, Seq[(Type, Identifier)])): Seq[Formal] = parsedInfo match {
    case (hType, hName, tail) => Formal(hType, hName) +: tail.map({ case(type_, name) => Formal(type_, name)})
  }

  def formalList[_: P] = P(type_ ~ id ~ ("," ~ type_ ~ id).rep).?
    .map({
      case Some(parsedInfo) => flattenFormalList(parsedInfo)
      case None => Seq()
    })

  def methodDecl[_: P] = P("public" ~ type_ ~ id ~ "(" ~ formalList ~ ")" ~
    "{" ~/
      varDecl.rep ~
      stmt.rep ~
      "return" ~ expr ~ ";" ~
    "}")
    .map({
      case(type_, name, formals, varDecls, stmts, returnVal) =>
        MethodDecl(type_, name, formals, varDecls, stmts, returnVal)
    })

  def classDecl[_: P] = P("class" ~ id ~ "{" ~ varDecl.rep ~ methodDecl.rep ~ "}")
    .map({ case(name, varDecls, methodDecls) => ClassDecl(name, varDecls, methodDecls) })

  def mainClass[_: P] = P("class" ~ id ~ "{" ~
      "public" ~ "static" ~ "void" ~ "main" ~ "(" ~ "String" ~ "[" ~ "]" ~ id ~ ")" ~ "{" ~
        varDecl.rep ~
        stmt.rep ~
      "}" ~
    "}")
    .map({ case(name, stdArgsName, varDecls, stmts) => MainClass(name, stdArgsName, varDecls, stmts)})

  def program[_: P] = P(mainClass ~ classDecl.rep ~ End)
    .map({ case(mainClass, classDecls) => Program(mainClass, classDecls) })

  def parse(s: String, debug: Boolean = false): Either[ParseError, Program] = {
    val result = fastparse.parse(s, program(_), verboseFailures = debug)
    result match {
      case f: Parsed.Failure =>
        if (debug) println(f.longMsg)
        Left(ParseError(f.label))
      case Parsed.Success(value, _) =>
        Right(value)
    }
  }

  def parse_(s: String, debug: Boolean = false): Parsed[Program] = {
    val result = fastparse.parse(s, program(_), verboseFailures = debug)
    result match {
      case p: Parsed.Failure => println(p.longMsg)
      case _ =>
    }
    result
  }
}