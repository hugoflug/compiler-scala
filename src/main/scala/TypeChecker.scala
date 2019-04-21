import SymbolTableCreator.SymbolTable

object TypeChecker {

  abstract class TypeError(msg: String) extends CompilerError(msg)
  case class WrongTypeError(actualType: Type, expectedType: Type) extends TypeError("TODO")
  case class WrongArgumentAmountError(msg: String) extends TypeError(msg)
  case class UndefinedNameError(msg: String) extends TypeError(msg)

  def typeCheck(program: Program, symTable: SymbolTable): Option[TypeError] = {
    (for {
      _ <- typeCheck(program.mainClass, symTable)
      _ <- typeCheckAll(program.classDecls, symTable)
    } yield ()).swap.toOption
  }

  def assertTypeExistence(typeName: Type, symTable: SymbolTable): Either[TypeError, Unit] =
    typeName match {
      case ObjectType(name) =>
        if (symTable.contains(name)) Right()
        else Left(UndefinedNameError("No type named: " + name))
      case _ => Right()
    }

  def assertType(expr: Expr, expected: Type, symTable: SymbolTable): Either[TypeError, Unit] =
    for {
      type_ <- typeCheck(expr, symTable)
      _ <- assertType(type_, expected)
    } yield ()

  def assertType(actual: Type, expected: Type): Either[TypeError, Unit] =
    if (actual == expected) Right()
    else Left(WrongTypeError(actual, expected))

  def assertAnyType(actual: Type, expected: Seq[Type]): Either[TypeError, Unit] =
    if (expected.contains(actual)) Right()
    else Left(WrongTypeError(actual, expected.head))
    // TODO: make WrongTypeError be able to take multiple expected types

  // TODO: de-uglify
//  def assertTypes(result: Type, typeAsserts: (Expr, Type)*): Either[TypeError, Type] = {
//    val lefts = typeAsserts.map(t => assertType(t._1, t._2)).collect({ case left: Left[TypeError, Type] => left })
//    if (lefts.nonEmpty) lefts.head
//    else Right(result)
//  }

  def typeCheckAll(nodes: Seq[SyntaxTreeNode], symTable: SymbolTable): Either[TypeError, Unit] = {
    val lefts = nodes.map(n => typeCheck(n, symTable)).collect({ case left: Left[TypeError, Type] => left })
    if (lefts.nonEmpty) lefts.head
    else Right()
  }

  def assertBinOp(b: BinaryOp, leftOpType: Type, rightOpType: Type, resultType: Type, symTable: SymbolTable) =
    for {
      _ <- assertType(b.leftOp, leftOpType, symTable)
      _ <- assertType(b.rightOp, rightOpType, symTable)
    } yield resultType

  def typeCheck(treeNode: SyntaxTreeNode, symTable: SymbolTable): Either[TypeError, Unit] =
    treeNode match {
      case m: MainClass =>
        for {
          _ <- typeCheckAll(m.varDecls, symTable)
          _ <- typeCheckAll(m.stmts, symTable)
        } yield ()
      case c: ClassDecl =>
        for {
          _ <- typeCheckAll(c.varDecls, symTable)
          _ <- typeCheckAll(c.methodDecls, symTable)
        } yield ()
      case v: VarDecl =>
        assertTypeExistence(v.typeName, symTable)
      case m: MethodDecl =>
        for {
          _ <- typeCheckAll(m.argList, symTable)
          _ <- typeCheckAll(m.varDeclList, symTable)
          _ <- typeCheckAll(m.stmts, symTable)
          _ <- assertType(m.returnVal, m.typeName, symTable)
        } yield ()
      case s: Syso =>
        for {
          printeeType <- typeCheck(s.printee, symTable)
          _ <- assertAnyType(printeeType, Seq(IntType(), BooleanType()))
        } yield ()
      case w: While =>
        for {
          _ <- assertType(w.condition, BooleanType(), symTable)
          _ <- typeCheck(w.stmt, symTable)
        } yield ()
      case i: If =>
        for {
          _ <- assertType(i.condition, BooleanType(), symTable)
          _ <- typeCheck(i.thenStmt, symTable)
          _ <- typeCheck(i.elseStmt, symTable)
        } yield ()
      case i: IfWithoutElse =>
        for {
          _ <- assertType(i.condition, BooleanType(), symTable)
          _ <- typeCheck(i.thenStmt, symTable)
        } yield ()
      case f: Formal =>
        for {
          _ <- assertTypeExistence(f.typeName, symTable)
        } yield f.typeName // TODO: ???
    }

  def typeCheck(expr: Expr, symTable: SymbolTable): Either[TypeError, Type] =
    expr match {
      case e @ And(_, _) | Or(_, _) =>
        assertBinOp(e, BooleanType(), BooleanType(), BooleanType(), symTable)
      case e @ Plus(_, _) | Minus(_, _) | Mult(_, _) =>
        assertBinOp(e, IntType(), IntType(), IntType(), symTable)
      case e @ GreaterThan(_, _) | GreaterOrEqualThan(_, _) | LessThan(_, _) | LessOrEqualThan(_,_) =>
        assertBinOp(e, IntType(), IntType(), BooleanType(), symTable)
      case e @ Equal(_, _) | NotEqual(_, _) =>
        for {
          leftOpType <- typeCheck(e.leftOp, symTable)
          rightOpType <- typeCheck(e.rightOp, symTable)
          _ <- assertType(leftOpType, rightOpType)
        } yield BooleanType()
      case a: ArrayLength =>
        for {
          _ <- assertType(a.array, IntArrayType(), symTable)
        } yield IntType()
      case a: ArrayLookup =>
        for {
          _ <- assertType(a.array, IntArrayType(), symTable)
          _ <- assertType(a.index, IntType(), symTable)
        } yield IntType()
      case n: NewObject =>
        val name = n.typeName.name
        if (symTable.contains(name)) Right(ObjectType(name))
        else Left(UndefinedNameError("undefined name: " + name))
      case n: NewArray =>
        for {
          _ <- assertType(n.arraySize, IntType(), symTable)
        } yield IntArrayType()
      case n: Not =>
        for {
          _ <- assertType(n.expr, BooleanType(), symTable)
        } yield BooleanType()
      case p: Parens =>
        typeCheck(p.expr, symTable)
      case _: False =>
        Right(BooleanType())
      case _: True =>
        Right(BooleanType())
      case _: IntLit =>
        Right(IntType())
    }
}
