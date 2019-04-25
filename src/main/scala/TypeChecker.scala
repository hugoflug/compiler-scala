import Compiler.CompilationError
import SymbolTableCreator.{ClassTable, MethodTable, SymbolTable}
import EitherUtils.orFirstError

object TypeChecker {
  abstract class TypeError(override val index: Int) extends CompilationError(index)
  case class WrongTypeError(actualType: Type, expectedType: Type, override val index: Int) extends TypeError(index)
  case class TypeNotInListError(actualType: Type, expectedTypes: Seq[Type], override val index: Int) extends TypeError(index)
  case class WrongArgumentAmountError(actual: Int, expected: Int, override val index: Int) extends TypeError(index)
  case class UndefinedNameError(name: String, override val index: Int) extends TypeError(index)
  case class IntSizeError(size: Long, override val index: Int) extends TypeError(index)
  case class MultidimArrayError(override val index: Int) extends TypeError(index)

  case class Context(symTable: SymbolTable, currentClass: Option[ClassTable], currentMethod: Option[MethodTable])

  abstract class Type
  case class BooleanType() extends Type
  case class IntArrayType() extends Type
  case class IntType() extends Type
  case class ObjectType(name: String) extends Type
  case class Void() extends Type

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
        case And(_, _, _) | Or(_, _, _) =>
          assertBinOp(b, BooleanType(), BooleanType(), BooleanType(), c)
        case Plus(_, _, _) | Minus(_, _, _) | Mult(_, _, _) =>
          assertBinOp(b, IntType(), IntType(), IntType(), c)
        case GreaterThan(_, _, _) | GreaterOrEqualThan(_, _, _) | LessThan(_, _, _) | LessOrEqualThan(_, _, _) =>
          assertBinOp(b, IntType(), IntType(), BooleanType(), c)
        case Equal(_, _, _) | NotEqual(_, _, _) =>
          for {
            leftOpType <- typeCheck(b.leftOp, c)
            rightOpType <- typeCheck(b.rightOp, c)
            _ <- assertType(leftOpType, rightOpType, b.index)
          } yield BooleanType()
      }
      case a: ArrayLength =>
        for {
          _ <- assertType(a.array, IntArrayType(), c)
        } yield IntType()
      case a: ArrayLookup =>
        for {
          _ <- assertType(a.array, IntArrayType(), c)
          _ <- assertType(a.arrayIndex, IntType(), c)
          _ <- assertNotNewArray(a.array)
        } yield IntType()
      case n: NewObject =>
        val name = n.typeName.name
        if (c.symTable.contains(name)) Right(ObjectType(name))
        else Left(UndefinedNameError(name, n.index))
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
          .toRight(UndefinedNameError(i.name, i.index))
      case p: Parens =>
        typeCheck(p.expr, c)
      case _: False =>
        Right(BooleanType())
      case _: True =>
        Right(BooleanType())
      case IntLit(value, index) =>
        if (value > Int.MaxValue) Left(IntSizeError(value, index))
        else Right(IntType())
      case call: MethodCall =>
        for {
          objType <- typeCheck(call.obj, c)
          _ <- assertIsObjectType(objType, c, call.obj.index)
          cObjType = objType.asInstanceOf[ObjectType]
          argTypes <- orFirstError(call.args.map(typeCheck(_, c)))
          returnType <- getMethodReturnType(cObjType.name, call.methodName.name, argTypes, c.symTable, call.index)
        } yield returnType
      case t: This =>
        if (c.currentClass.isEmpty) Left(UndefinedNameError("this", t.index))
        else Right(ObjectType(c.currentClass.get.name))
    }

  def typeOfNode(typeNode: TypeNode) = typeNode match {
    case _: IntTypeNode =>
      IntType()
    case _: BooleanTypeNode =>
      BooleanType()
    case _: IntArrayTypeNode =>
      IntArrayType()
    case ObjectTypeNode(name, _) =>
      ObjectType(name)
  }

  private def assertTypeExists(typeName: Type, symTable: SymbolTable, index: Int) =
    typeName match {
      case ObjectType(name) =>
        if (symTable.contains(name)) Right()
        else Left(UndefinedNameError(name, index))
      case _ => Right()
    }

  private def assertIsObjectType(t: Type, context: Context, index: Int) =
    if (t.isInstanceOf[ObjectType]) Right()
    else Left(WrongTypeError(t, ObjectType(""), index))

  private def assertTypeListEq(expectedTypes: Seq[Type], actualTypes: Seq[Type], index: Int) =
    expectedTypes.zip(actualTypes).find(p => p._1 != p._2) match {
      case Some((expected, actual)) => Left(WrongTypeError(actual, expected, index))
      case None => Right()
    }

  private def assertArgAmountEq(expectedArgCount: Int, actualArgCount: Int, index: Int) =
    if (expectedArgCount == actualArgCount) Right()
    else Left(WrongArgumentAmountError(actualArgCount, expectedArgCount, index))

  private def getMethodReturnType(className: String, methodName: String, params: Seq[Type], symTable: SymbolTable,
                                  index: Int) =
    for {
      classTable <- symTable.get(className).toRight(UndefinedNameError(className, index))
      methodTable <- classTable.methods.get(methodName).toRight(UndefinedNameError(methodName, index))
      expectedParams = methodTable.params.values.toSeq.sortBy(_.varNo)
      _ <- assertArgAmountEq(expectedParams.length, params.length, index)
      _ <- assertTypeListEq(expectedParams.map(_.type_), params, index)
    } yield methodTable.returnType

  private def assertNotNewArray(expr: Expr): Either[TypeError, Unit] = expr match {
    case NewArray(_, index) => Left(MultidimArrayError(index))
    case _ => Right()
  }
  private def assertType(expr: Expr, expected: Type, context: Context): Either[TypeError, Unit] =
    for {
      type_ <- typeCheck(expr, context)
      _ <- assertType(type_, expected, expr.index)
    } yield ()

  private def assertType(actual: Type, expected: Type, index: Int) =
    if (actual == expected) Right()
    else Left(WrongTypeError(actual, expected, index))

  private def assertOneOfTypes(actual: Type, expected: Seq[Type], index: Int) =
    if (expected.contains(actual)) Right()
    else Left(TypeNotInListError(actual, expected, index))

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
          _ <- assertType(m.returnVal, typeOfNode(m.typeName), newContext)
        } yield ()
      case v: VarDecl =>
        assertTypeExists(typeOfNode(v.typeName), c.symTable, v.index)
      case s: Syso =>
        for {
          printeeType <- typeCheck(s.printee, c)
          _ <- assertOneOfTypes(printeeType, Seq(IntType(), BooleanType()), s.printee.index)
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
        assertTypeExists(typeOfNode(f.typeName), c.symTable, f.index)
      case b: Block =>
        typeCheckAll(b.stmtList, c)
      case a: Assign =>
        for {
          assigneeType <- typeCheck(a.assignee, c)
          newValType <- typeCheck(a.newValue, c)
          _ <- assertType(assigneeType, newValType, a.assignee.index)
        } yield ()
      case a: ArrayAssign =>
        for {
          _<- assertType(a.array, IntArrayType(), c)
          _<- assertType(a.arrayIndex, IntType(), c)
          _<- assertType(a.newValue, IntType(), c)
        } yield ()
    }
}
