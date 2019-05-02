import SymbolTableCreator.{SymbolTable, Var}
import TypeChecker.{BooleanType, Context, IntArrayType, IntType, ObjectType, Type, VoidType, typeOfNode}

object CodeGenerator {

  def generate(program: Program, symTable: SymbolTable): Seq[JVMClass] =
    genMainClass(program.mainClass, symTable) +: program.classDecls.map(genClass(_, symTable))

  private case class CodegenContext(instructions: Seq[JVMInstruction], currentLabel: Int) {
    def flatMap(f: Int => CodegenContext): CodegenContext = {
      val result = f(currentLabel)
      CodegenContext(instructions ++ result.instructions, result.currentLabel)
    }

    def >>>(f: Int => CodegenContext): CodegenContext = flatMap(f)

    def >>(i: JVMInstruction): CodegenContext = CodegenContext(instructions :+ i, currentLabel)
  }

  private def asm(currentLabel: Int): CodegenContext = CodegenContext(Seq(), currentLabel)

  private def oneOf[T](options: Option[T]*): Option[T] =
    options.find(_.isDefined).flatten

  private def genMainClass(classDecl: MainClass, symTable: SymbolTable): JVMClass = {
    val classTable = symTable(classDecl.name.name)
    val methodTable = classTable.methods("main")
    val context = Context(symTable, Some(classTable), Some(methodTable))

    JVMClass(
      className = classDecl.name.name,
      superClass = "java/lang/Object",
      fields = Seq(),
      methods = Seq(JVMMethod(
        name = "main",
        typeDesc = methodTypeDescriptor(Seq(), VoidType()),
        maxStack = StackDepthCalculator.maxStackDepth(classDecl.stmts) + 1,
        maxLocals = methodTable.params.size + methodTable.locals.size + 1,
        code = genAll(classDecl.stmts, context)(0).instructions
      ))
    )
  }

  private def genClass(classDecl: ClassDecl, symTable: SymbolTable): JVMClass = {
    val classTable = symTable(classDecl.name.name)
    val context = Context(symTable, Some(classTable), None)

    JVMClass(
      className = classDecl.name.name,
      superClass = "java/lang/Object",
      fields = classDecl.varDecls.map(genField),
      methods = classDecl.methodDecls.map(genMethod(_, context))
    )
  }

  private def genField(varDecl: VarDecl) : JVMField =
    JVMField(varDecl.name.name, typeDescriptor(typeOfNode(varDecl.typeName)))

  private def genMethod(method: MethodDecl, context: Context): JVMMethod = {
    val methodTable = context.symTable(context.currentClass.get.name).methods(method.name.name)
    val c = context.copy(currentMethod = Some(methodTable))

    JVMMethod(
      name = method.name.name,
      typeDesc = methodTypeDescriptor(method.argList.map(arg => typeOfNode(arg.typeName)), typeOfNode(method.typeName)),
      maxStack = StackDepthCalculator.maxStackDepth(method.stmts) + 1,
      maxLocals = methodTable.params.size + methodTable.locals.size + 1,
      code = genAll(method.stmts, c)(0).instructions
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

  private def genAll(nodes: Seq[SyntaxTreeNode], c: Context)(label: Int): CodegenContext =
    nodes.map(n => gen(n, c)(_)).foldLeft(asm(label)) { _ >>> _ }

  private def genBinaryOp(binOp: BinaryOp, instruction: JVMInstruction, c: Context)(label: Int) =
    asm(label) >>> gen(binOp.leftOp, c) >>> gen(binOp.rightOp, c) >> instruction

  private def genComparisonOp(binOp: BinaryOp, compareInstr: Int => InstructionWithLabel, c: Context)(label: Int) = {
    val setTrue = label
    val after = label + 1
    asm(label + 2) >>>
      gen(binOp.leftOp, c) >>>
      gen(binOp.rightOp, c) >>
      compareInstr(setTrue) >>
      Iconst_0() >>
      Goto(after) >>
      Label(setTrue) >>
      Iconst_1() >>
      Label(after)
  }

  private def genShortCircuitOp(binOp: BinaryOp, compareInstr: Int => InstructionWithLabel, c: Context)(label: Int) =
    asm(label + 1) >>>
      gen(binOp.leftOp, c) >>
      Dup() >>
      compareInstr(label) >>
      Pop() >>>
      gen(binOp.rightOp, c) >>
      Label(label)

  private def genAssign(assignee: String, methodVar: Option[Var], c: Context)(label: Int) =
    methodVar match {
      case Some(value) => value match {
        case Var(_, IntType(), varNo) =>
          asm(label) >> Istore(varNo)
        case Var(_, BooleanType(), varNo) =>
          asm(label) >> Istore(varNo)
        case Var(_, _, varNo) =>
          asm(label) >> Astore(varNo)
      }
      case None =>
        val clazz = c.currentClass.get
        val type_ = clazz.fields(assignee).type_
        val typeDesc = typeDescriptor(type_)
        asm(label) >> Aload_0() >> Swap() >> Putfield(clazz.name, assignee, typeDesc)
    }

  private def gen(node: SyntaxTreeNode, c: Context)(label: Int): CodegenContext =
    node match {
      case ArrayAssign(array, index, newValue, _) =>
        asm(label) >>> gen(array, c) >>> gen(index, c) >>> gen(newValue, c) >> Iastore()

      case Assign(Identifier(assignee, _), newValue, _) =>
        val method = c.currentMethod.get
        val methodVar = oneOf(method.locals.get(assignee), method.params.get(assignee))
        asm(label) >>> gen(newValue, c) >>> genAssign(assignee, methodVar, c)

      case Block(stmtList, _) =>
        asm(label) >>> genAll(stmtList, c)

      case While(condition, stmt, _) =>
        val start = label
        val after = label + 1
        asm(label + 2) >>
          Label(start) >>>
          gen(condition, c) >>
          Ifeq(after) >>>
          gen(stmt, c) >>
          Goto(start) >>
          Label(after)

      case Syso(printee, _) =>
        val printeeType = TypeChecker.getType(printee, c)
        asm(label) >>
          Getstatic("System", "out", "Ljava/io/PrintStream;") >>>
          gen(printee, c) >>>
          (label =>
            printeeType match {
              case IntType() =>
                asm(label) >>
                  Invokevirtual("java/io/PrintStream", "println", "(I)V")
              case _ =>
                val falze = label
                val after = label + 1
                asm(label + 2) >>
                  Ifeq(falze) >>
                  LdcString("true") >>
                  Goto(after) >>
                  Label(falze) >>
                  LdcString("false") >>
                  Label(after) >>
                  Invokevirtual("java/io/PrintStream", "invokevirtual", "(Ljava/lang/String;)V")
            })

      case If(condition, thenStmt, elseStmt, _) =>
        val lbl = label
        val after = label + 1
        asm(label + 2) >>>
          gen(condition, c) >>
          Ifeq(lbl) >>>
          gen(thenStmt, c) >>
          Goto(after) >>
          Label(lbl) >>>
          gen(elseStmt, c) >>
          Label(after)

      case IfWithoutElse(condition, thenStmt, _) =>
        val lbl = label
        asm(label + 1) >>>
          gen(condition, c) >>
          Ifeq(lbl) >>>
          gen(thenStmt, c) >>
          Label(lbl)

      case p: Plus =>
        asm(label) >>> genBinaryOp(p, Iadd(), c)

      case m: Minus =>
        asm(label) >>> genBinaryOp(m, Isub(), c)

      case g: GreaterThan =>
        asm(label) >>> genComparisonOp(g, If_icmpgt, c)

      case g: GreaterOrEqualThan =>
        asm(label) >>> genComparisonOp(g, If_icmpge, c)

      case lt: LessThan =>
        asm(label) >>> genComparisonOp(lt, If_icmplt, c)

      case leq: LessOrEqualThan =>
        asm(label) >>> genComparisonOp(leq, If_icmple, c)

      case m: Mult =>
        asm(label) >>> genBinaryOp(m, Imul(), c)

      case IntLit(value, _) =>
        asm(label) >> LdcInt(value.toInt)

      case Not(e, _) =>
        asm(label) >>> gen(e, c) >> Iconst_1() >> Ixor()

      case NewArray(arraySize, _) =>
        asm(label) >>> gen(arraySize, c) >> New_array(10)

      case NewObject(Identifier(typeName, _), _) =>
        asm(label) >> New(typeName) >> Dup() >> Invokespecial(typeName, "<init>", "()V")

      case ArrayLength(array, _) =>
        asm(label) >>> gen(array, c) >> Array_length()

      case ArrayLookup(array, index, _) =>
        asm(label) >>> gen(array, c) >>> gen(index, c) >> Iaload()

      case False(_) =>
        asm(label) >> Iconst_0()

      case True(_) =>
        asm(label) >> Iconst_1()

      case This(_) =>
        asm(label) >> Aload_0()

      case or: Or =>
        asm(label) >>> genShortCircuitOp(or, Ifne, c)

      case and: And =>
        asm(label) >>> genShortCircuitOp(and, Ifeq, c)

      case MethodCall(obj, methodName, args, _) =>
        val objType = TypeChecker.getType(obj, c).asInstanceOf[ObjectType]
        val returnType = c.symTable(objType.name).methods(methodName.name).returnType
        val argTypeList = args.map(TypeChecker.getType(_, c))
        val typeDesc = methodTypeDescriptor(argTypeList, returnType)
        asm(label) >>> gen(obj, c) >>> genAll(args, c) >> Invokevirtual(objType.name, methodName.name, typeDesc)

      case e@Equal(_, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => If_acmpeq
          case _ => If_icmpeq
        }
        asm(label) >>> genComparisonOp(e, compareInstruct, c)

      case ne@NotEqual(_, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => If_acmpne
          case _ => If_icmpne
        }
        asm(label) >>> genComparisonOp(ne, compareInstruct, c)

      case Parens(e, _) =>
        asm(label) >>> gen(e, c)

      case Identifier(name, _) =>
        val method = c.currentMethod.get
        val localVar = oneOf(method.locals.get(name), method.params.get(name))

        localVar match {
          case Some(value) => value match {
            case Var(_, IntType(), varNo) =>
              asm(label) >> Iload(varNo)
            case Var(_, BooleanType(), varNo) =>
              asm(label) >> Iload(varNo)
            case Var(_, _, varNo) =>
              asm(label) >> Aload(varNo)
          }
          case None =>
            val clazz = c.currentClass.get
            val type_ = clazz.fields(name).type_
            val typeDesc = typeDescriptor(type_)
            asm(label) >> Aload_0() >> Getfield(clazz.name, name, typeDesc)
        }

      case _ => asm(label)
  }
}
