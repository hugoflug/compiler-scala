import SymbolTableCreator.{SymbolTable, Var}
import TypeChecker.{BooleanType, Context, IntArrayType, IntType, ObjectType, Type, typeOfNode}

object CodeGenerator {
  case class JasminAssembly(filename: String, program: String)

  def generate(program: Program, symTable: SymbolTable, sourceFile: String): Seq[JasminAssembly] =
    gen(program.mainClass, symTable, sourceFile) +: program.classDecls.map(gen(_, symTable, sourceFile))

  private case class CodegenResult(program: String, currentLabel: Int) {
    def flatMap(f: Int => CodegenResult): CodegenResult = {
      val result = f(currentLabel)
      CodegenResult(program + result.program, result.currentLabel)
    }

    def map(f: Int => Int): CodegenResult =
      CodegenResult(program, f(currentLabel))

    def <++>(f: Int => CodegenResult): CodegenResult = flatMap(f)

    def <+>(s: String): CodegenResult = CodegenResult(program + s + "\n", currentLabel)
  }

  private def asm(currentLabel: Int): CodegenResult = CodegenResult("", currentLabel)

  private def esc(s: String) = "'" + s + "'"

  private def genBinaryOp(binOp: BinaryOp, instruction: String, c: Context)(label: Int) =
    asm(label) <++> gen(binOp.leftOp, c) <++> gen(binOp.rightOp, c) <+> instruction

  private def genComparison(compareInstr: String)(label: Int) = {
    val setTrue = label
    val after = label + 1
    asm(label + 2) <+>
      compareInstr + " l" + setTrue <+>
      "iconst_0" <+>
      "goto l" + after <+>
      "l" + setTrue + ":" <+>
      "iconst_1" <+>
      "l" + after + ":"
  }

  private def genShortCircuitOp(binOp: BinaryOp, compareInstr: String, c: Context)(label: Int) =
    asm(label + 1) <++>
      gen(binOp.leftOp, c) <+>
      "dup" <+>
      compareInstr + " l" + label <+>
      "pop" <++>
      gen(binOp.rightOp, c) <+>
      "end: l" + label

  private def oneOf[T](options: Option[T]*): Option[T] =
    if (options.isEmpty) None
    else options.head.orElse(oneOf(options.tail:_*))

  private def typeDescriptor(t: Type): String = t match {
      case ObjectType(name) => "L" + name + ";"
      case IntType() => "I"
      case IntArrayType() => "[I"
      case BooleanType() => "I"
    }

  private def methodDescriptor(className: String, methodName: String, types: Seq[Type], returnType: Type) =
    className + "/" + methodName + "(" + types.map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

  private def methodDescriptor(methodName: String, formals: Seq[Formal], returnType: Type) =
    methodName + "(" + formals.map(t => typeOfNode(t.typeName))
      .map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

  private def genAll(nodes: Seq[SyntaxTreeNode], c: Context)(label: Int): CodegenResult =
    nodes.map(n => gen(n, c)(_)).foldLeft(asm(label))(_ <++> _)

  private def gen(classDecl: MainClass, symTable: SymbolTable, sourceFile: String): JasminAssembly = {
    val classTable = symTable(classDecl.name.name)
    val methodTable = classTable.methods("main")
    val context = Context(symTable, Some(classTable), Some(methodTable))
    val maxStack = 500 // TODO: calculate
    val codeGenResult = asm(0) <+>
      ".source " + esc(sourceFile) <+>
      ".class public " + esc(classDecl.name.name) <+>
      ".super java/lang/Object" <+>
      ".method public static main([Ljava/lang/String;)V" <+>
      ".limit stack " + maxStack <+>
      ".limit locals " + (methodTable.params.size + methodTable.locals.size + 1) <++>
      genAll(classDecl.varDecls, context) <++>
      genAll(classDecl.stmts, context) <+>
      "return" <+>
      ".end method"

    JasminAssembly(classDecl.name.name + ".jasmin", codeGenResult.program)
  }

  private def gen(classDecl: ClassDecl, symTable: SymbolTable, sourceFile: String): JasminAssembly = {
    val context = Context(symTable, symTable.get(classDecl.name.name), None)
    val codeGenResult = asm(0) <+>
      ".source " + esc(sourceFile) <+>
      ".class public " + esc(classDecl.name.name) <+>
      ".super java/lang/Object" <+>
      classDecl.varDecls.map(
        v => ".field public " + esc(v.name.name) + " " + esc(typeDescriptor(typeOfNode(v.typeName)))).mkString <+>
      ".method public <init>()V" <+>
      ".limit stack 1" <+>
      ".limit locals 1" <+>
      "aload_0" <+>
      "invokespecial java/lang/Object/<init>( <+>V" <+>
      "return" <+>
      ".end method" <++>
      genAll(classDecl.methodDecls, context)

    JasminAssembly(classDecl.name.name + ".jasmin", codeGenResult.program)
  }

  private def gen(node: SyntaxTreeNode, c: Context)(label: Int): CodegenResult =
    node match {
      case ArrayAssign(array, index, newValue, _) =>
        asm(label) <++> gen(array, c) <++> gen(index, c) <++> gen(newValue, c) <+> "iastore"

      case Assign(Identifier(assignee, _), newValue, _) =>
        val method = c.currentMethod.get
        val methodVars = oneOf(method.locals.get(assignee), method.params.get(assignee))

        asm(label) <++> gen(newValue, c) <++>
          (label =>
            methodVars match {
              case Some(value) => value match {
                case Var(_, IntType(), varNo) =>
                  asm(label) <+> "istore " + varNo
                case Var(_, BooleanType(), varNo) =>
                  asm(label) <+> "istore " + varNo
                case Var(_, _, varNo) =>
                  asm(label) <+> "astore " + varNo
              }
              case None =>
                val clazz = c.currentClass.get
                val type_ = clazz.fields(assignee).type_
                val fieldDesc = clazz.name + "/" + assignee
                val typeDesc = typeDescriptor(type_)
                asm(label) <+> "aload_0" <+> "swap" <+> "putfield " + esc(fieldDesc) + " " + esc(typeDesc)
            })

      case Block(stmtList, _) =>
        asm(label) <++> genAll(stmtList, c)

      case MethodDecl(type_, Identifier(name, _), argList, varDeclList, stmts, returnVal, _) =>
        val methodTable = c.symTable(c.currentClass.get.name).methods(name)
        val newContext = c.copy(currentMethod = Some(methodTable))
        val methodDescr = methodDescriptor(name, argList, methodTable.returnType)
        val maxStack = 500 //TODO: calculate
        asm(label) <+>
          ".method public " + esc(methodDescr) <+>
          ".limit stack " + maxStack <+>
          ".limit locals " + (methodTable.params.size + methodTable.locals.size + 1) <++>
          genAll(argList, newContext) <++>
          genAll(varDeclList, newContext) <++>
          genAll(stmts, newContext) <++>
          gen(returnVal, newContext) <+>
          (type_ match {
            case IntTypeNode(_) | BooleanTypeNode(_) => "ireturn"
            case _ => "areturn"
          }) <+>
          ".end method"

      case While(condition, stmt, _) =>
        val start = label
        val after = label + 1
        asm(label + 2) <+>
          "l" + start + ":" <++>
          gen(condition, c) <+>
          "ifeq l" + after <++>
          gen(stmt, c) <+>
          "goto l" + start <+>
          "l" + after + ":"

      case Syso(printee, _) =>
        val printeeType = TypeChecker.getType(printee, c)
        asm(label) <+>
          "getstatic java/lang/System/out Ljava/io/PrintStream;" <++>
          gen(printee, c) <++>
          (label =>
            printeeType match {
              case IntType() =>
                asm(label) <+>
                  "invokevirtual java/io/PrintStream/println(I)V"
              case _ =>
                val falze = label
                val after = label + 1
                asm(label + 2) <+>
                  "ifeq l" + falze <+>
                  "ldc \"true\"" <+>
                  "goto l" + after <+>
                  "l" + falze + ":" <+>
                  "ldc \"false\"" <+>
                  "l" + after + ":" <+>
                  "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
            })

      case _ => asm(label)
    }

  private def gen(expr: Expr, c: Context)(label: Int): CodegenResult =
    expr match {
      case p: Plus =>
        asm(label) <++> genBinaryOp(p, "iadd", c)
      case m: Minus =>
        asm(label) <++> genBinaryOp(m, "isub", c)
      case GreaterThan(leftOp, rightOp, _) =>
        asm(label) <++> gen(leftOp, c) <++> gen(rightOp, c) <++> genComparison("if_icmpgt")
      case GreaterOrEqualThan(leftOp, rightOp, _) =>
        asm(label) <++> gen(leftOp, c) <++> gen(rightOp, c) <++> genComparison("if_icmpge")
      case LessThan(leftOp, rightOp, _) =>
        asm(label) <++> gen(leftOp, c) <++> gen(rightOp, c) <++> genComparison("if_icmplt")
      case LessOrEqualThan(leftOp, rightOp, _) =>
        asm(label) <++> gen(leftOp, c) <++> gen(rightOp, c) <++> genComparison("if_icmple")
      case m: Mult =>
        asm(label) <++> genBinaryOp(m, "imul", c)
      case IntLit(value, _) =>
        asm(label) <+> "ldc " + value.toString
      case Not(e, _) =>
        asm(label) <++> gen(e, c) <+> "iconst_1" <+> "ixor"
      case NewArray(arraySize, _) =>
        asm(label) <++> gen(arraySize, c) <+> "newarray int"
      case NewObject(Identifier(typeName,_), _) =>
        asm(label) <+> "new " + esc(typeName) <+> "dup" <+> "invokespecial " + esc(typeName + "/<init>()V")
      case ArrayLength(array, _) =>
        asm(label) <++> gen(array, c) <+> "arraylength"
      case ArrayLookup(array, index, _) =>
        asm(label) <++> gen(array, c) <++> gen(index, c) <+> "iaload"
      case False(_) =>
        asm(label) <+> "iconst_0"
      case True(_) =>
        asm(label) <+> "iconst_1"
      case This(_) =>
        asm(label) <+> "aload_0"
      case or: Or =>
        asm(label) <++> genShortCircuitOp(or, "ifne", c)
      case and: And =>
        asm(label) <++> genShortCircuitOp(and, "ifeq", c)
      case MethodCall(obj, methodName, args, _) =>
        val objType = TypeChecker.getType(obj, c).asInstanceOf[ObjectType]
        val returnType = c.symTable(objType.name).methods(methodName.name).returnType
        val argTypeList = args.map(TypeChecker.getType(_, c))
        val methodDesc = methodDescriptor(objType.name, methodName.name, argTypeList, returnType)
        asm(label) <++> gen(obj, c) <++> genAll(args, c) <+> "invokevirtual " + esc(methodDesc)
      case Equal(leftOp, rightOp, _) =>
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => "if_acmpeq"
          case _ => "if_icmpeq"
        }
        asm(label) <++> gen(leftOp, c) <++> gen(rightOp, c) <++> genComparison(compareInstruct)
      case NotEqual(leftOp, rightOp, _) => // TODO: code duplication
        val compareInstruct = TypeChecker.getType(rightOp, c) match {
          case ObjectType(_) | IntArrayType() => "if_acmpne"
          case _ => "if_icmpne"
        }
        asm(label) <++> gen(leftOp, c) <++> gen(rightOp, c) <++> genComparison(compareInstruct)
      case Parens(e, _) =>
        asm(label) <++> gen(e, c)
      case _ => asm(label)
    }
}
