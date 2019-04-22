import SymbolTableCreator.{ClassTable, MethodTable, SymbolTable}

object TypeChecker {
  type TypeErr = Either[TypeError, Unit]
  
  abstract class TypeError(msg: String) extends CompilerError(msg)
  case class WrongTypeError(actualType: Type, expectedType: Type) extends TypeError("TODO")
  case class WrongArgumentAmountError(msg: String) extends TypeError(msg)
  case class UndefinedNameError(msg: String) extends TypeError(msg)

  case class Context(symTable: SymbolTable, currentClass: Option[ClassTable], currentMethod: Option[MethodTable])

  def typeCheck(program: Program, symTable: SymbolTable): TypeErr = {
    val context = Context(symTable, None, None)
    for {
      _ <- typeCheck(program.mainClass, context)
      _ <- typeCheckAll(program.classDecls, context)
    } yield ()
  }

  def assertTypeExistence(typeName: Type, symTable: SymbolTable): TypeErr =
    typeName match {
      case ObjectType(name) =>
        if (symTable.contains(name)) Right()
        else Left(UndefinedNameError("No type named: " + name))
      case _ => Right()
    }

  def assertType(expr: Expr, expected: Type, context: Context): TypeErr =
    for {
      type_ <- typeCheck(expr, context)
      _ <- assertType(type_, expected)
    } yield ()

  def assertType(actual: Type, expected: Type): TypeErr =
    if (actual == expected) Right()
    else Left(WrongTypeError(actual, expected))

  def assertAnyType(actual: Type, expected: Seq[Type]): TypeErr =
    if (expected.contains(actual)) Right()
    else Left(WrongTypeError(actual, expected.head))
    // TODO: make WrongTypeError be able to take multiple expected types

  // TODO: de-uglify
//  def assertTypes(result: Type, typeAsserts: (Expr, Type)*): Either[TypeError, Type] = {
//    val lefts = typeAsserts.map(t => assertType(t._1, t._2)).collect({ case left: Left[TypeError, Type] => left })
//    if (lefts.nonEmpty) lefts.head
//    else Right(result)
//  }

  def typeCheckAll(nodes: Seq[SyntaxTreeNode], context: Context): TypeErr = {
    val lefts = nodes.map(n => typeCheck(n, context)).collect({ case left: Left[TypeError, Type] => left })
    if (lefts.nonEmpty) lefts.head
    else Right()
  }

  def assertBinOp(b: BinaryOp, leftOpType: Type, rightOpType: Type, resultType: Type, context: Context) =
    for {
      _ <- assertType(b.leftOp, leftOpType, context)
      _ <- assertType(b.rightOp, rightOpType, context)
    } yield resultType

  def oneOf[T](options: Option[T]*): Option[T] =
    if (options.isEmpty) None
    else options.head.orElse(oneOf(options.tail:_*))

  def typeCheck(treeNode: SyntaxTreeNode, c: Context): TypeErr =
    treeNode match {
      case m: MainClass =>
        val newContext = c.copy(currentClass = Some(c.symTable(m.name.name)),
          currentMethod = Some(c.currentClass.get.methods("main")))
        for {
          _ <- typeCheckAll(m.varDecls, newContext)
          _ <- typeCheckAll(m.stmts, newContext)
        } yield ()
      case cd: ClassDecl =>
        val newContext = c.copy(currentClass = Some(c.symTable(cd.name.name)))
        for {
          _ <- typeCheckAll(cd.varDecls, newContext)
          _ <- typeCheckAll(cd.methodDecls, newContext)
        } yield ()
      case m: MethodDecl =>
        val newContext = c.copy(currentMethod = Some(c.currentClass.get.methods(m.name)))
        for {
          _ <- typeCheckAll(m.argList, newContext)
          _ <- typeCheckAll(m.varDeclList, newContext)
          _ <- typeCheckAll(m.stmts, newContext)
          _ <- assertType(m.returnVal, m.typeName, newContext)
        } yield ()
      case v: VarDecl =>
        assertTypeExistence(v.typeName, c.symTable)
      case s: Syso =>
        for {
          printeeType <- typeCheck(s.printee, c)
          _ <- assertAnyType(printeeType, Seq(IntType(), BooleanType()))
        } yield ()
      case w: While =>
        for {
          _ <- assertType(w.condition, BooleanType(), c)
          _ <- typeCheck(w.stmt, c)
        } yield ()
      case i: If =>
        for {
          _ <- assertType(i.condition, BooleanType(), c)
          _ <- typeCheck(i.thenStmt, c)
          _ <- typeCheck(i.elseStmt, c)
        } yield ()
      case i: IfWithoutElse =>
        for {
          _ <- assertType(i.condition, BooleanType(), c)
          _ <- typeCheck(i.thenStmt, c)
        } yield ()
      case f: Formal =>
        assertTypeExistence(f.typeName, c.symTable)
      case b: Block =>
        typeCheckAll(b.stmtList, c)
      case a: Assign =>
        for {
          assigneeType <- typeCheck(a.assignee, c)
          newValType <- typeCheck(a.newValue, c)
          _ <- assertType(assigneeType, newValType)
        } yield ()
      case a: ArrayAssign =>
        for {
          _ <- assertType(a.array, IntArrayType(), c)
          _ <- assertType(a.index, IntType(), c)
          _ <- assertType(a.newValue, IntType(), c)
        } yield ()
    }

  def typeCheck(expr: Expr, c: Context): Either[TypeError, Type] =
    expr match {
      case e @ And(_, _) | Or(_, _) =>
        assertBinOp(e, BooleanType(), BooleanType(), BooleanType(), c)
      case e @ Plus(_, _) | Minus(_, _) | Mult(_, _) =>
        assertBinOp(e, IntType(), IntType(), IntType(), c)
      case e @ GreaterThan(_, _) | GreaterOrEqualThan(_, _) | LessThan(_, _) | LessOrEqualThan(_,_) =>
        assertBinOp(e, IntType(), IntType(), BooleanType(), c)
      case e @ Equal(_, _) | NotEqual(_, _) =>
        for {
          leftOpType <- typeCheck(e.leftOp, c)
          rightOpType <- typeCheck(e.rightOp, c)
          _ <- assertType(leftOpType, rightOpType)
        } yield BooleanType()
      case a: ArrayLength =>
        for {
          _ <- assertType(a.array, IntArrayType(), c)
        } yield IntType()
      case a: ArrayLookup =>
        for {
          _ <- assertType(a.array, IntArrayType(), c)
          _ <- assertType(a.index, IntType(), c)
        } yield IntType()
      case n: NewObject =>
        val name = n.typeName.name
        if (c.symTable.contains(name)) Right(ObjectType(name))
        else Left(UndefinedNameError("undefined name: " + name))
      case n: NewArray =>
        for {
          _ <- assertType(n.arraySize, IntType(), c)
        } yield IntArrayType()
      case n: Not =>
        for {
          _ <- assertType(n.expr, BooleanType(), c)
        } yield BooleanType()
      case i: Identifier =>
        val method = c.currentMethod.get
        val type_ = oneOf(method.locals.get(i.name),
          method.params.get(i.name),
          c.currentClass.get.fields.get(i.name))
        type_ match {
          case Some(t) => Right(t)
          case None => Left(UndefinedNameError("undefined name: " + i.name))
        }
      case p: Parens =>
        typeCheck(p.expr, c)
      case _: False =>
        Right(BooleanType())
      case _: True =>
        Right(BooleanType())
      case _: IntLit =>
        Right(IntType())
    }
}
