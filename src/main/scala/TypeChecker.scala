import SymbolTableCreator.{ClassTable, MethodTable, SymbolTable}
import EitherUtils.{orErr, assert, firstError}

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
      case call: MethodCall =>
        for {
          objType <- typeCheck(call.obj, c)
          _ <- assertIsObjectType(objType, c)
          cObjType = objType.asInstanceOf[ObjectType]
          argTypes <- firstError(call.args.map(typeCheck(_, c)))
          returnType <- getMethodReturnType(cObjType.name, call.methodName.name, argTypes, c.symTable)
        } yield returnType
      case _: This =>
        if (c.currentClass.isDefined) Left(UndefinedNameError("undefined name: this"))
        else Right(ObjectType(c.currentClass.get.name))
    }

  private def assertTypeExists(typeName: Type, symTable: SymbolTable): TypeErr =
    typeName match {
      case ObjectType(name) =>
        if (symTable.contains(name)) Right()
        else Left(UndefinedNameError("No type named: " + name))
      case _ => Right()
    }

  private def getMethodReturnType(className: String, methodName: String, params: Seq[Type], symTable: SymbolTable) =
    for {
      classTable <- orErr(symTable.get(className),
        UndefinedNameError("No type named: " + className))
      methodTable <- orErr(classTable.methods.get(methodName),
        UndefinedNameError("No method named: " + methodName + " in " + className))
      _ <- assert(methodTable.params.keys.toSeq.length == params.length,
        WrongArgumentAmountError("Wrong number of parameters: " + methodName + " in " + className))
      _ <- assert(methodTable.params.values.toSeq == params,
        UndefinedNameError("Parameter mismatch: " + methodName + " in " + className)) // TODO: improve error message
    } yield methodTable.returnType

  private def assertType(expr: Expr, expected: Type, context: Context): TypeErr =
    for {
      type_ <- typeCheck(expr, context)
      _ <- assertType(type_, expected)
    } yield ()

  private def assertIsObjectType(t: Type, context: Context): TypeErr =
    if (t.isInstanceOf[ObjectType]) Right()
    else Left(WrongTypeError(t, ObjectType("")))

  private def assertType(actual: Type, expected: Type): TypeErr =
    if (actual == expected) Right()
    else Left(WrongTypeError(actual, expected))

  private def assertAnyType(actual: Type, expected: Seq[Type]): TypeErr =
    if (expected.contains(actual)) Right()
    else Left(WrongTypeError(actual, expected.head))
    // TODO: make WrongTypeError be able to take multiple expected types

  private def typeCheckAll(nodes: Seq[SyntaxTreeNode], context: Context): TypeErr = {
    val lefts = nodes.map(n => typeCheck(n, context)).collect({ case left: Left[TypeError, Unit] => left })
    if (lefts.nonEmpty) lefts.head
    else Right()
  }

  private def assertBinOp(b: BinaryOp, leftOpType: Type, rightOpType: Type, resultType: Type, context: Context) =
    for {
      _ <- assertType(b.leftOp, leftOpType, context)
      _ <- assertType(b.rightOp, rightOpType, context)
    } yield resultType

  private def oneOf[T](options: Option[T]*): Option[T] =
    if (options.isEmpty) None
    else options.head.orElse(oneOf(options.tail:_*))

  private def typeCheck(treeNode: SyntaxTreeNode, c: Context): TypeErr =
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
