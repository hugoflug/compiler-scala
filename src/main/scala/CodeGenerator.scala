import SymbolTableCreator.{SymbolTable, Var}
import TypeChecker.{BooleanType, Context, IntArrayType, IntType, ObjectType, Type, VoidType, typeOfNode}

object CodeGenerator {
  case class JasminAssembly(className: String, assembly: String)

  def generate(program: Program, symTable: SymbolTable, sourceFile: String): Seq[JasminAssembly] =
    gen(program.mainClass, symTable, sourceFile) +: program.classDecls.map(gen(_, symTable, sourceFile))

  private case class CodegenContext(program: String, currentLabel: Int) {
    def flatMap(f: Int => CodegenContext): CodegenContext = {
      val result = f(currentLabel)
      CodegenContext(program + result.program, result.currentLabel)
    }

    def map(f: Int => Int): CodegenContext =
      CodegenContext(program, f(currentLabel))

    def >>>(f: Int => CodegenContext): CodegenContext = flatMap(f)

    def >>(s: String): CodegenContext = CodegenContext(program + s + "\n", currentLabel)
  }

  private def asm(currentLabel: Int): CodegenContext = CodegenContext("", currentLabel)

  private def esc(s: String) = "'" + s + "'"

  private def l(i: Int) = "l" + i

  private def oneOf[T](options: Option[T]*): Option[T] =
    options.find(_.isDefined).flatten

  private def typeDescriptor(t: Type): String = t match {
      case ObjectType(name) => "L" + name + ";"
      case IntType() => "I"
      case IntArrayType() => "[I"
      case BooleanType() => "I"
      case VoidType() => ""
    }

  private def methodDescriptor(className: String, methodName: String, types: Seq[Type], returnType: Type) =
    className + "/" + methodName + "(" + types.map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

  private def methodDescriptor(methodName: String, formalTypes: Seq[Type], returnType: Type) =
    methodName + "(" + formalTypes.map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

  private def genAll(nodes: Seq[SyntaxTreeNode], c: Context)(label: Int): CodegenContext =
    nodes.map(n => gen(n, c)(_)).foldLeft(asm(label)) { _ >>> _ }

  private def genBinaryOp(binOp: BinaryOp, instruction: String, c: Context)(label: Int) =
    asm(label) >>> gen(binOp.leftOp, c) >>> gen(binOp.rightOp, c) >> instruction

  private def genComparisonOp(binOp: BinaryOp, compareInstr: String, c: Context)(label: Int) = {
    val setTrue = label
    val after = label + 1
    asm(label + 2) >>>
      gen(binOp.leftOp, c) >>>
      gen(binOp.rightOp, c) >>
      compareInstr + " " + l(setTrue) >>
      "iconst_0" >>
      "goto " + l(after) >>
      l(setTrue) + ":" >>
      "iconst_1" >>
      l(after) + ":"
  }

  private def genShortCircuitOp(binOp: BinaryOp, compareInstr: String, c: Context)(label: Int) =
    asm(label + 1) >>>
      gen(binOp.leftOp, c) >>
      "dup" >>
      compareInstr + " " + l(label) >>
      "pop" >>>
      gen(binOp.rightOp, c) >>
      l(label) + ":"

  private def genAssign(assignee: String, methodVar: Option[Var], c: Context)(label: Int) =
    methodVar match {
      case Some(value) => value match {
        case Var(_, IntType(), varNo) =>
          asm(label) >> "istore " + varNo
        case Var(_, BooleanType(), varNo) =>
          asm(label) >> "istore " + varNo
        case Var(_, _, varNo) =>
          asm(label) >> "astore " + varNo
      }
      case None =>
        val clazz = c.currentClass.get
        val type_ = clazz.fields(assignee).type_
        val fieldDesc = clazz.name + "/" + assignee
        val typeDesc = typeDescriptor(type_)
        asm(label) >> "aload_0" >> "swap" >> "putfield " + esc(fieldDesc) + " " + esc(typeDesc)
    }

  private def gen(classDecl: MainClass, symTable: SymbolTable, sourceFile: String): JasminAssembly = {
    val classTable = symTable(classDecl.name.name)
    val methodTable = classTable.methods("main")
    val context = Context(symTable, Some(classTable), Some(methodTable))
    val maxStack = StackDepthCalculator.maxStackDepth(classDecl.stmts) + 1
    val codeGenResult = asm(0) >>
      ".source " + esc(sourceFile) >>
      ".class public " + esc(classDecl.name.name) >>
      ".super java/lang/Object" >>
      ".method public static main([Ljava/lang/String;)V" >>
      ".limit stack " + maxStack >>
      ".limit locals " + (methodTable.params.size + methodTable.locals.size + 1) >>>
      genAll(classDecl.varDecls, context) >>>
      genAll(classDecl.stmts, context) >>
      "return" >>
      ".end method"

    JasminAssembly(classDecl.name.name, codeGenResult.program)
  }

  private def gen(classDecl: ClassDecl, symTable: SymbolTable, sourceFile: String): JasminAssembly = {
    val context = Context(symTable, symTable.get(classDecl.name.name), None)
    val codeGenResult = asm(0) >>
      ".source " + esc(sourceFile) >>
      ".class public " + esc(classDecl.name.name) >>
      ".super java/lang/Object" >>
      classDecl.varDecls.map(
        v => ".field public " + esc(v.name.name) + " " + esc(typeDescriptor(typeOfNode(v.typeName)))).mkString("\n") >>
      ".method public <init>()V" >>
      ".limit stack 1" >>
      ".limit locals 1" >>
      "aload_0" >>
      "invokespecial java/lang/Object/<init>()V" >>
      "return" >>
      ".end method" >>>
      genAll(classDecl.methodDecls, context)

    val b = ""

    JasminAssembly(classDecl.name.name, codeGenResult.program)
  }

  private def gen(node: SyntaxTreeNode, c: Context)(label: Int): CodegenContext =
    node match {
      case ArrayAssign(array, index, newValue, _) =>
        asm(label) >>> gen(array, c) >>> gen(index, c) >>> gen(newValue, c) >> "iastore"

      case Assign(Identifier(assignee, _), newValue, _) =>
        val method = c.currentMethod.get
        val methodVar = oneOf(method.locals.get(assignee), method.params.get(assignee))
        asm(label) >>> gen(newValue, c) >>> genAssign(assignee, methodVar, c)

      case Block(stmtList, _) =>
        asm(label) >>> genAll(stmtList, c)

      case decl @ MethodDecl(type_, Identifier(name, _), argList, varDeclList, stmts, returnVal, _) =>
        val methodTable = c.symTable(c.currentClass.get.name).methods(name)
        val newContext = c.copy(currentMethod = Some(methodTable))
        val argTypeList = argList.map(a => typeOfNode(a.typeName))
        val methodDescr = methodDescriptor(name, argTypeList, methodTable.returnType)
        val maxStack = StackDepthCalculator.maxStackDepth(decl) + 1
        asm(label) >>
          ".method public " + esc(methodDescr) >>
          ".limit stack " + maxStack >>
          ".limit locals " + (methodTable.params.size + methodTable.locals.size + 1) >>>
          genAll(argList, newContext) >>>
          genAll(varDeclList, newContext) >>>
          genAll(stmts, newContext) >>>
          gen(returnVal, newContext) >>
          (type_ match {
            case IntTypeNode(_) | BooleanTypeNode(_) => "ireturn"
            case _ => "areturn"
          }) >>
          ".end method"

      case While(condition, stmt, _) =>
        val start = label
        val after = label + 1
        asm(label + 2) >>
          l(start) + ":" >>>
          gen(condition, c) >>
          "ifeq " + l(after) >>>
          gen(stmt, c) >>
          "goto " + l(start) >>
          l(after) + ":"

      case Syso(printee, _) =>
        val printeeType = TypeChecker.getType(printee, c)
        asm(label) >>
          "getstatic java/lang/System/out Ljava/io/PrintStream;" >>>
          gen(printee, c) >>>
          (label =>
            printeeType match {
              case IntType() =>
                asm(label) >>
                  "invokevirtual java/io/PrintStream/println(I)V"
              case _ =>
                val falze = label
                val after = label + 1
                asm(label + 2) >>
                  "ifeq " + l(falze) >>
                  "ldc \"true\"" >>
                  "goto " + l(after) >>
                  l(falze) + ":" >>
                  "ldc \"false\"" >>
                  l(after) + ":" >>
                  "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
            })

      case If(condition, thenStmt, elseStmt, _) =>
        val lbl = label
        val after = label + 1
        asm(label + 2) >>>
          gen(condition, c) >>
          "ifeq " + l(lbl) >>>
          gen(thenStmt, c) >>
          "goto " + l(after) >>
          l(lbl) + ":" >>>
          gen(elseStmt, c) >>
          l(after) + ":"

      case IfWithoutElse(condition, thenStmt, _) =>
        val lbl = label
        asm(label + 1) >>>
          gen(condition, c) >>
          "ifeq " + l(lbl) >>>
          gen(thenStmt, c) >>
          l(lbl) + ":"

      case p: Plus =>
        asm(label) >>> genBinaryOp(p, "iadd", c)

      case m: Minus =>
        asm(label) >>> genBinaryOp(m, "isub", c)

      case g: GreaterThan =>
        asm(label) >>> genComparisonOp(g, "if_icmpgt", c)

      case g: GreaterOrEqualThan =>
        asm(label) >>> genComparisonOp(g, "if_icmpge", c)

      case lt: LessThan =>
        asm(label) >>> genComparisonOp(lt, "if_icmplt", c)

      case leq: LessOrEqualThan =>
        asm(label) >>> genComparisonOp(leq, "if_icmple", c)

      case m: Mult =>
        asm(label) >>> genBinaryOp(m, "imul", c)

      case IntLit(value, _) =>
        asm(label) >> "ldc " + value.toString

      case Not(e, _) =>
        asm(label) >>> gen(e, c) >> "iconst_1" >> "ixor"

      case NewArray(arraySize, _) =>
        asm(label) >>> gen(arraySize, c) >> "newarray int"

      case NewObject(Identifier(typeName,_), _) =>
        asm(label) >> "new " + esc(typeName) >> "dup" >> "invokespecial " + esc(typeName + "/<init>()V")

      case ArrayLength(array, _) =>
        asm(label) >>> gen(array, c) >> "arraylength"

      case ArrayLookup(array, index, _) =>
        asm(label) >>> gen(array, c) >>> gen(index, c) >> "iaload"

      case False(_) =>
        asm(label) >> "iconst_0"

      case True(_) =>
        asm(label) >> "iconst_1"

      case This(_) =>
        asm(label) >> "aload_0"

      case or: Or =>
        asm(label) >>> genShortCircuitOp(or, "ifne", c)

      case and: And =>
        asm(label) >>> genShortCircuitOp(and, "ifeq", c)

      case MethodCall(obj, methodName, args, _) =>
        val objType = TypeChecker.getType(obj, c).asInstanceOf[ObjectType]
        val returnType = c.symTable(objType.name).methods(methodName.name).returnType
        val argTypeList = args.map(TypeChecker.getType(_, c))
        val methodDesc = methodDescriptor(objType.name, methodName.name, argTypeList, returnType)
        asm(label) >>> gen(obj, c) >>> genAll(args, c) >> "invokevirtual " + esc(methodDesc)

      case e @ Equal(_, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => "if_acmpeq"
          case _ => "if_icmpeq"
        }
        asm(label) >>> genComparisonOp(e, compareInstruct, c)

      case ne @ NotEqual(_, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => "if_acmpne"
          case _ => "if_icmpne"
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
              asm(label) >> "iload " + varNo
            case Var(_, BooleanType(), varNo) =>
              asm(label) >> "iload " + varNo
            case Var(_, _, varNo) =>
              asm(label) >> "aload " + varNo
          }
          case None =>
            val clazz = c.currentClass.get
            val type_ = clazz.fields(name).type_
            val fieldDesc = clazz.name + "/" + name
            val typeDesc = typeDescriptor(type_)
            asm(label) >> "aload_0" >> "getfield " + esc(fieldDesc) + " " + esc(typeDesc)
        }

      case _ => asm(label)
    }
}
