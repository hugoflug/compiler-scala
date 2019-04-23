import SymbolTableCreator.{ClassTable, MethodTable, SymbolTable}
import EitherUtils.orFirstError

object TypeChecker {
  abstract class TypeError extends CompilerError
  case class WrongTypeError(actualType: Type, expectedType: Type) extends TypeError
  case class TypeNotInListError(actualType: Type, expectedTypes: Seq[Type]) extends TypeError
  case class WrongArgumentAmountError(actual: Int, expected: Int) extends TypeError
  case class UndefinedNameError(name: String) extends TypeError

  case class Context(symTable: SymbolTable, currentClass: Option[ClassTable], currentMethod: Option[MethodTable])

  def typeCheck(program: Program, symTable: SymbolTable): Either[TypeError, Unit] = {
    val context = Context(symTable, None, None)
    for {
      _ <- typeCheck(program.mainClass, context)
      _ <- typeCheckAll(program.classDecls, context)
    } yield ()
  }

  def getType(expr: Expr, c: Context): Type =
    typeCheck(expr, c).right.get

  def typeCheck(expr: Expr, c: Context): Either[TypeError, Type] =
    expr match {
      case b: BinaryOp => b match {
        case And(_, _) | Or(_, _) =>
          assertBinOp(b, BooleanType(), BooleanType(), BooleanType(), c)
        case Plus(_, _) | Minus(_, _) | Mult(_, _) =>
          assertBinOp(b, IntType(), IntType(), IntType(), c)
        case GreaterThan(_, _) | GreaterOrEqualThan(_, _) | LessThan(_, _) | LessOrEqualThan(_, _) =>
          assertBinOp(b, IntType(), IntType(), BooleanType(), c)
        case Equal(_, _) | NotEqual(_, _) =>
          for {
            leftOpType <- typeCheck(b.leftOp, c)
            rightOpType <- typeCheck(b.rightOp, c)
            _ <- assertType(leftOpType, rightOpType)
          } yield BooleanType()
      }
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
        else Left(UndefinedNameError(name))
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
        val classFields = c.currentClass.get.fields
        oneOf(method.locals.get(i.name), method.params.get(i.name), classFields.get(i.name))
          .map(_.type_)
          .toRight(UndefinedNameError(i.name))
      case p: Parens =>
        typeCheck(p.expr, c)
      case _: False =>
        Right(BooleanType())
      case _: True =>
        Right(BooleanType())
      case _: IntLit =>
        Right(IntType())
      case call: MethodCall =>
        for {
          objType <- typeCheck(call.obj, c)
          _ <- assertIsObjectType(objType, c)
          cObjType = objType.asInstanceOf[ObjectType]
          argTypes <- orFirstError(call.args.map(typeCheck(_, c)))
          returnType <- getMethodReturnType(cObjType.name, call.methodName.name, argTypes, c.symTable)
        } yield returnType
      case _: This =>
        if (c.currentClass.isDefined) Left(UndefinedNameError("this"))
        else Right(ObjectType(c.currentClass.get.name))
    }

  private def assertTypeExists(typeName: Type, symTable: SymbolTable) =
    typeName match {
      case ObjectType(name) =>
        if (symTable.contains(name)) Right()
        else Left(UndefinedNameError(name))
      case _ => Right()
    }

  private def assertIsObjectType(t: Type, context: Context) =
    if (t.isInstanceOf[ObjectType]) Right()
    else Left(WrongTypeError(t, ObjectType("")))

  private def assertTypeListEq(expectedTypes: Seq[Type], actualTypes: Seq[Type]) =
    expectedTypes.zip(actualTypes).find(p => p._1 != p._2) match {
      case Some((expected, actual)) => Left(WrongTypeError(actual, expected))
      case None => Right()
    }

  private def assertArgAmountEq(expectedArgCount: Int, actualArgCount: Int) =
    if (expectedArgCount == actualArgCount) Right()
    else Left(WrongArgumentAmountError(actualArgCount, expectedArgCount))

  private def getMethodReturnType(className: String, methodName: String, params: Seq[Type], symTable: SymbolTable) =
    for {
      classTable <- symTable.get(className).toRight(UndefinedNameError(className))
      methodTable <- classTable.methods.get(methodName).toRight(UndefinedNameError(methodName))
      expectedParams = methodTable.params.values.toSeq
      _ <- assertArgAmountEq(expectedParams.length, params.length)
      _ <- assertTypeListEq(expectedParams.map(_.type_), params)
    } yield methodTable.returnType

  private def assertType(expr: Expr, expected: Type, context: Context): Either[TypeError, Unit] =
    for {
      type_ <- typeCheck(expr, context)
      _ <- assertType(type_, expected)
    } yield ()

  private def assertType(actual: Type, expected: Type) =
    if (actual == expected) Right()
    else Left(WrongTypeError(actual, expected))

  private def assertOneOfTypes(actual: Type, expected: Seq[Type]) =
    if (expected.contains(actual)) Right()
    else Left(TypeNotInListError(actual, expected))

  private def typeCheckAll(nodes: Seq[SyntaxTreeNode], context: Context) =
    nodes.map(typeCheck(_, context)).find(_.isLeft) match {
      case Some(left) => left
      case None => Right()
    }

  private def assertBinOp(b: BinaryOp, leftOpType: Type, rightOpType: Type, resultType: Type, context: Context) =
    for {
      _ <- assertType(b.leftOp, leftOpType, context)
      _ <- assertType(b.rightOp, rightOpType, context)
    } yield resultType

  private def oneOf[T](options: Option[T]*): Option[T] =
    if (options.isEmpty) None
    else options.head.orElse(oneOf(options.tail:_*))

  private def typeCheck(treeNode: SyntaxTreeNode, c: Context): Either[TypeError, Unit] =
    treeNode match {
      case m: MainClass =>
        val newContext = c.copy(currentClass = Some(c.symTable(m.name.name)),
          currentMethod = Some(c.symTable(m.name.name).methods("main")))
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
        val newContext = c.copy(currentMethod = Some(c.currentClass.get.methods(m.name.name)))
        for {
          _ <- typeCheckAll(m.argList, newContext)
          _ <- typeCheckAll(m.varDeclList, newContext)
          _ <- typeCheckAll(m.stmts, newContext)
          _ <- assertType(m.returnVal, m.typeName, newContext)
        } yield ()
      case v: VarDecl =>
        assertTypeExists(v.typeName, c.symTable)
      case s: Syso =>
        for {
          printeeType <- typeCheck(s.printee, c)
          _ <- assertOneOfTypes(printeeType, Seq(IntType(), BooleanType()))
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
        assertTypeExists(f.typeName, c.symTable)
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
          _<- assertType(a.array, IntArrayType(), c)
          _<- assertType(a.index, IntType(), c)
          _<- assertType(a.newValue, IntType(), c)
        } yield ()
    }
}
