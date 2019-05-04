import SymbolTableCreator.{SymbolTable, Var}
import TypeChecker.{BooleanType, Context, IntArrayType, IntType, ObjectType, Type, VoidType, typeOfNode}

import scala.util.hashing.MurmurHash3

object CodeGenerator {

  def generate(program: Program, symTable: SymbolTable): Seq[JVMClass] =
    genMainClass(program.mainClass, symTable) +: program.classDecls.map(genClass(_, symTable))

  private implicit class XSeq[A](val x: Seq[A]) extends AnyVal {
    def +++[B >: A, That](elem: B) = x :+ elem
  }

  private def oneOf[T](options: Option[T]*): Option[T] =
    options.find(_.isDefined).flatten

  private def genMainClass(classDecl: MainClass, symTable: SymbolTable): JVMClass = {
    val classTable = symTable(classDecl.name.name)
    val methodTable = classTable.methods("main")
    val context = Context(symTable, Some(classTable), Some(methodTable), 0)

    JVMClass(
      className = classDecl.name.name,
      superClass = "java/lang/Object",
      fields = Seq(),
      methods = Seq(JVMMethod(
        name = "main",
        typeDesc = mainMethodTypeDescriptor,
        static = true,
        maxStack = StackDepthCalculator.maxStackDepth(classDecl.stmts) + 1,
        maxLocals = methodTable.params.size + methodTable.locals.size + 1,
        code = genMainMethodCode(classDecl.stmts, context)
      ))
    )
  }

  private def genClass(classDecl: ClassDecl, symTable: SymbolTable): JVMClass = {
    val classTable = symTable(classDecl.name.name)
    val context = Context(symTable, Some(classTable), None, 0)

    JVMClass(
      className = classDecl.name.name,
      superClass = "java/lang/Object",
      fields = classDecl.varDecls.map(genField),
      methods = classDecl.methodDecls.map(genMethod(_, static = false, context)) :+ emptyConstructor
    )
  }

  private def emptyConstructor =
    JVMMethod(
      name = "<init>",
      typeDesc = "()V",
      static = false,
      maxStack = 1,
      maxLocals = 1,
      code = Seq(Aload_0(), Invokespecial("java/lang/Object", "<init>", "()V"), Return())
    )

  private def genField(varDecl: VarDecl) : JVMField =
    JVMField(varDecl.name.name, typeDescriptor(typeOfNode(varDecl.typeName)))

  private def hasDuplicateLabel(nodes: Seq[JVMInstruction]): Boolean =
    nodes.collect({ case Label(id) => id }).groupBy(identity).exists(_._2.length > 1)

  private def genMainMethodCode(stmts: Seq[Stmt], c: Context): Seq[JVMInstruction] = {
    val code = genAll(stmts, c, 0) ++ Seq(Return())

    if (hasDuplicateLabel(code)) {
      genMainMethodCode(stmts, c.copy(guid = c.guid + 1))
    } else {
      code
    }
  }

  private def genCode(method: MethodDecl, c: Context): Seq[JVMInstruction] = {
    val code = genAll(method.stmts, c, 0) ++
      gen(method.returnVal, c, 1) +++
      (method.typeName match {
        case IntTypeNode(_) | BooleanTypeNode(_) => Ireturn()
        case _ => Areturn()
      })

    if (hasDuplicateLabel(code)) {
      genCode(method, c.copy(guid = c.guid + 1))
    } else {
      code
    }
  }

  private def genMethod(method: MethodDecl, static: Boolean, context: Context): JVMMethod = {
    val methodTable = context.symTable(context.currentClass.get.name).methods(method.name.name)
    val c = context.copy(currentMethod = Some(methodTable))

    JVMMethod(
      name = method.name.name,
      typeDesc = methodTypeDescriptor(method.argList.map(arg => typeOfNode(arg.typeName)), typeOfNode(method.typeName)),
      static = static,
      maxStack = StackDepthCalculator.maxStackDepth(method.stmts :+ method.returnVal) + 1,
      maxLocals = methodTable.params.size + methodTable.locals.size + 1,
      code = genCode(method, c)
    )
  }

  private def typeDescriptor(t: Type): String = t match {
    case ObjectType(name) => "L" + name + ";"
    case IntType() => "I"
    case IntArrayType() => "[I"
    case BooleanType() => "I"
    case VoidType() => "V"
  }

  private def methodTypeDescriptor(types: Seq[Type], returnType: Type): String =
    "(" + types.map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

  private def mainMethodTypeDescriptor = "([Ljava/lang/String;)V"

  private def genAll(nodes: Seq[SyntaxTreeNode], c: Context, childNo: Int): Seq[JVMInstruction] = {
    val newCtx = c.copy(guid = MurmurHash3.mix(c.guid, childNo))
    nodes.zipWithIndex.flatMap({ case (n, i) => gen(n, newCtx, i) })
  }

  private def genBinaryOp(binOp: BinaryOp, instruction: JVMInstruction, c: Context) =
    gen(binOp.leftOp, c, 0) ++ gen(binOp.rightOp, c, 1) +++ instruction

  private def genComparisonOp(binOp: BinaryOp, compareInstr: Int => InstructionWithLabel, c: Context) = {
    val setTrue = c.guid
    val after = c.guid + 1

    gen(binOp.leftOp, c, 0) ++
    gen(binOp.rightOp, c, 1) +++
    compareInstr(setTrue) +++
    Iconst_0() +++
    Goto(after) +++
    Label(setTrue) +++
    Iconst_1() +++
    Label(after)
  }

  private def genShortCircuitOp(binOp: BinaryOp, compareInstr: Int => InstructionWithLabel, c: Context): Seq[JVMInstruction] = {
    val label = c.guid
    gen(binOp.leftOp, c, 0) +++
    Dup() +++
    compareInstr(label) +++
    Pop() ++
    gen(binOp.rightOp, c, 1) +++
    Label(label)
  }

  private def genAssign(assignee: String, methodVar: Option[Var], c: Context): Seq[JVMInstruction] =
    methodVar match {
      case Some(value) => value match {
        case Var(_, IntType(), varNo) =>
          Seq(Istore(varNo))
        case Var(_, BooleanType(), varNo) =>
          Seq(Istore(varNo))
        case Var(_, _, varNo) =>
          Seq(Astore(varNo))
      }
      case None =>
        val clazz = c.currentClass.get
        val type_ = clazz.fields(assignee).type_
        val typeDesc = typeDescriptor(type_)
        Seq(Aload_0(), Swap(), Putfield(clazz.name, assignee, typeDesc))
    }

  private def gen(node: SyntaxTreeNode, c: Context, childNo: Int): Seq[JVMInstruction] = {
    gen(node, c.copy(guid = MurmurHash3.mix(c.guid, childNo)))
  }

  private def gen(node: SyntaxTreeNode, c: Context): Seq[JVMInstruction] =
    node match {
      case ArrayAssign(array, index, newValue, _) =>
        gen(array, c, 0) ++ gen(index, c, 1) ++ gen(newValue, c, 2) +++ Iastore()

      case Assign(Identifier(assignee, _), newValue, _) =>
        val method = c.currentMethod.get
        val methodVar = oneOf(method.locals.get(assignee), method.params.get(assignee))
        gen(newValue, c, 0) ++ genAssign(assignee, methodVar, c)

      case Block(stmtList, _) =>
        genAll(stmtList, c, 0)

      case While(condition, stmt, _) =>
        val start = c.guid
        val after = c.guid + 1

        Seq() +++
        Label(start) ++
        gen(condition, c, 0) +++
        Ifeq(after) ++
        gen(stmt, c, 1) +++
        Goto(start) +++
        Label(after)

      case Syso(printee, _) =>
        val printeeType = TypeChecker.getType(printee, c)

        Seq() +++
        Getstatic("java/lang/System", "out", "Ljava/io/PrintStream;") ++
        gen(printee, c, 0) ++
        (printeeType match {
            case IntType() =>
              Seq(Invokevirtual("java/io/PrintStream", "println", "(I)V"))
            case _ =>
              val falze = c.guid
              val after = c.guid + 1
              Seq() +++
              Ifeq(falze) +++
              Ldc_wString("true") +++
              Goto(after) +++
              Label(falze) +++
              Ldc_wString("false") +++
              Label(after) +++
              Invokevirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
        })

      case If(condition, thenStmt, elseStmt, _) =>
        val lbl = c.guid
        val after = c.guid + 1

        gen(condition, c, 0) +++
        Ifeq(lbl) ++
        gen(thenStmt, c, 1) +++
        Goto(after) +++
        Label(lbl) ++
        gen(elseStmt, c, 2) +++
        Label(after)

      case IfWithoutElse(condition, thenStmt, _) =>
        val lbl = c.guid

        gen(condition, c, 0) +++
        Ifeq(lbl) ++
        gen(thenStmt, c, 1) +++
        Label(lbl)

      case p: Plus =>
        genBinaryOp(p, Iadd(), c)

      case m: Minus =>
        genBinaryOp(m, Isub(), c)

      case g: GreaterThan =>
        genComparisonOp(g, If_icmpgt, c)

      case g: GreaterOrEqualThan =>
        genComparisonOp(g, If_icmpge, c)

      case lt: LessThan =>
        genComparisonOp(lt, If_icmplt, c)

      case leq: LessOrEqualThan =>
        genComparisonOp(leq, If_icmple, c)

      case m: Mult =>
        genBinaryOp(m, Imul(), c)

      case IntLit(value, _) =>
        Seq(Ldc_wInt(value.toInt))

      case Not(e, _) =>
        gen(e, c, 0) +++ Iconst_1() +++ Ixor()

      case NewArray(arraySize, _) =>
        gen(arraySize, c, 0) +++ New_array(10)

      case NewObject(Identifier(typeName, _), _) =>
        Seq(New(typeName), Dup(), Invokespecial(typeName, "<init>", "()V"))

      case ArrayLength(array, _) =>
        gen(array, c, 0) +++ Array_length()

      case ArrayLookup(array, index, _) =>
        gen(array, c, 0) ++ gen(index, c, 1) +++ Iaload()

      case False(_) =>
        Seq(Iconst_0())

      case True(_) =>
        Seq(Iconst_1())

      case This(_) =>
        Seq(Aload_0())

      case or: Or =>
        genShortCircuitOp(or, Ifne, c)

      case and: And =>
        genShortCircuitOp(and, Ifeq, c)

      case MethodCall(obj, methodName, args, _) =>
        val objType = TypeChecker.getType(obj, c).asInstanceOf[ObjectType]
        val returnType = c.symTable(objType.name).methods(methodName.name).returnType
        val argTypeList = args.map(TypeChecker.getType(_, c))
        val typeDesc = methodTypeDescriptor(argTypeList, returnType)
        gen(obj, c, 0) ++ genAll(args, c, 1) +++ Invokevirtual(objType.name, methodName.name, typeDesc)

      case e@Equal(_, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => If_acmpeq
          case _ => If_icmpeq
        }
        genComparisonOp(e, compareInstruct, c)

      case ne@NotEqual(_, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => If_acmpne
          case _ => If_icmpne
        }
        genComparisonOp(ne, compareInstruct, c)

      case Parens(e, _) =>
        gen(e, c, 0)

      case Identifier(name, _) =>
        val method = c.currentMethod.get
        val localVar = oneOf(method.locals.get(name), method.params.get(name))

        localVar match {
          case Some(value) => value match {
            case Var(_, IntType(), varNo) =>
              Seq(Iload(varNo))
            case Var(_, BooleanType(), varNo) =>
              Seq(Iload(varNo))
            case Var(_, _, varNo) =>
              Seq(Aload(varNo))
          }
          case None =>
            val clazz = c.currentClass.get
            val type_ = clazz.fields(name).type_
            val typeDesc = typeDescriptor(type_)
            Seq(Aload_0(), Getfield(clazz.name, name, typeDesc))
        }

      case _ => Seq()
  }
}
