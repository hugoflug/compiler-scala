import SymbolTableCreator.{SymbolTable, Var}
import TypeChecker.Context

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
      compareInstr + " " + setTrue <+>
      "iconst_0" <+>
      "goto " + after <+>
      setTrue + ":" <+>
      "iconst_1" <+>
      after + ":"
  }

  private def genShortCircuitOp(binOp: BinaryOp, compareInstr: String, c: Context)(label: Int) =
    asm(label + 1) <++>
      gen(binOp.leftOp, c) <+>
      "dup" <+>
      compareInstr + " " + label <+>
      "pop" <++>
      gen(binOp.rightOp, c) <+>
      "end: " + label

  private def oneOf[T](options: Option[T]*): Option[T] =
    if (options.isEmpty) None
    else options.head.orElse(oneOf(options.tail:_*))

  private def typeDescriptor(t: Type): String = t match {
      case ObjectType(name) => "L" + name + ";"
      case IntType() => "I"
      case IntArrayType() => "[I"
      case BooleanType() => "I"
    }

  private def methodDescriptor(className: String, methodName: String,  types: Seq[Type], returnType: Type) =
    className + "/" + methodName + "(" + types.map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

  private def methodDescriptor(methodName: String, formals: Seq[Formal], returnType: Type) =
    methodName + "(" + formals.map(_.typeName).map(typeDescriptor).mkString + ")" + typeDescriptor(returnType)

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

    JasminAssembly(classDecl.name.name, codeGenResult.program)
  }

  private def gen(classDecl: ClassDecl, symTable: SymbolTable, sourceFile: String): JasminAssembly = {
    val context = Context(symTable, symTable.get(classDecl.name.name), None)
    val codeGenResult = asm(0) <+>
      ".source " + esc(sourceFile) <+>
      ".class public " + esc(classDecl.name.name) <+>
      ".super java/lang/Object" <+>
      classDecl.varDecls.map(
        v => ".field public " + esc(v.name.name) + " " + esc(typeDescriptor(v.typeName))).mkString <+>
      ".method public <init>()V" <+>
      ".limit stack 1" <+>
      ".limit locals 1" <+>
      "aload_0" <+>
      "invokespecial java/lang/Object/<init>( <+>V" <+>
      "return" <+>
      ".end method" <++>
      genAll(classDecl.methodDecls, context)

    JasminAssembly(classDecl.name.name, codeGenResult.program)
  }

  private def gen(node: SyntaxTreeNode, c: Context)(label: Int): CodegenResult =
    node match {
      case a: ArrayAssign =>
        asm(label) <++> gen(a.array, c) <++> gen(a.index, c) <++> gen(a.newValue, c) <+> "iastore"
      case a: Assign =>
        val assignee = a.assignee.name
        val method = c.currentMethod.get
        val methodVars = oneOf(method.locals.get(assignee), method.params.get(assignee))
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
        }
      case b: Block =>
        asm(label) <++> genAll(b.stmtList, c)
      case m: MethodDecl =>
        val name = m.name.name
        val methodTable = c.symTable(c.currentClass.get.name).methods(name)
        val newContext = c.copy(currentMethod = Some(methodTable))
        val methodDescr = methodDescriptor(name, m.argList, methodTable.returnType)
        val maxStack = 500 //TODO: calculate
        asm(label) <+>
          ".method public " + esc(methodDescr) <+>
          ".limit stack " + maxStack <+>
          ".limit locals " + (methodTable.params.size + methodTable.locals.size + 1) <++>
          genAll(m.argList, newContext) <++>
          genAll(m.varDeclList, newContext) <++>
          genAll(m.stmts, newContext) <++>
          gen(m.returnVal, newContext) <+>
          (m.typeName match {
            case IntType() | BooleanType() => "ireturn"
            case _ => "areturn"
          }) <+>
          ".end method"
      case w: While =>
        val start = label
        val after = label + 1
        asm(label + 2) <+>
          start + ":" <++>
          gen(w.condition, c) <+>
          "ifeq: " + after <++>
          gen(w.stmt, c) <+>
          "goto " + start <+>
          after + ":"
      case s: Syso =>
        val printeeType = TypeChecker.getType(s.printee, c)
        asm(label) <+>
          "getstatic java/lang/System/out Ljava/io/PrintStream;" <++>
          gen(s.printee, c) <++>
          (label =>
            printeeType match {
              case IntType() =>
                asm(label) <+>
                  "invokevirtual java/io/PrintStream/println(I)V"
              case _ =>
                val falze = label
                val after = label + 1
                asm(label + 2) <+>
                  "ifeq " + falze <+>
                  "ldc \"true\"" <+>
                  "goto " + after <+>
                  falze + ":" <+>
                  "ldc \"false\"" <+>
                  after + ":" <+>
                  "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"
            })
      }

  private def gen(expr: Expr, c: Context)(label: Int): CodegenResult =
    expr match {
      case p: Plus =>
        asm(label) <++> genBinaryOp(p, "iadd", c)
      case m: Minus =>
        asm(label) <++> genBinaryOp(m, "isub", c)
      case gt: GreaterThan =>
        asm(label) <++> genBinaryOp(gt, "if_icmpgt", c)
      case get: GreaterOrEqualThan =>
        asm(label) <++> genBinaryOp(get, "if_icmpge", c)
      case lt: LessThan =>
        asm(label) <++> genBinaryOp(lt, "if_icmplt", c)
      case let: LessOrEqualThan =>
        asm(label) <++> genBinaryOp(let, "if_icmple", c)
      case m: Mult =>
        asm(label) <++> genBinaryOp(m, "imul", c)
      case i: IntLit =>
        asm(label) <+> "ldc " <+> i.value.toString
      case n: Not =>
        asm(label) <++> gen(n.expr, c) <+> "iconst_1" <+> "ixor"
      case n: NewArray =>
        asm(label) <++> gen(n.arraySize, c) <+> "newarray int"
      case n: NewObject =>
        val typeName = n.typeName.name
        asm(label) <+> "new " + esc(n.typeName.name) <+> "dup" <+> "invokespecial " + esc(typeName + "/<init>()V")
      case a: ArrayLength =>
        asm(label) <++> gen(a.array, c) <+> "arraylength"
      case a: ArrayLookup =>
        asm(label) <++> gen(a.array, c) <++> gen(a.index, c) <+> "iaload"
      case _: False =>
        asm(label) <+> "iconst_0"
      case _: True =>
        asm(label) <+> "iconst_1"
      case _: This =>
        asm(label) <+> "aload_0"
      case or: Or =>
        asm(label) <++> genShortCircuitOp(or, "ifne", c)
      case and: And =>
        asm(label) <++> genShortCircuitOp(and, "ifeq", c)
      case call: MethodCall =>
        val objType = TypeChecker.getType(call.obj, c).asInstanceOf[ObjectType]
        val returnType = c.symTable(objType.name).methods(call.methodName.name).returnType
        val argTypeList = call.args.map(TypeChecker.getType(_, c))
        val methodDesc = methodDescriptor(objType.name, call.methodName.name, argTypeList, returnType)
        asm(label) <++> gen(call.obj, c) <++> genAll(call.args, c) <+> "invokevirtual " + esc(methodDesc)
      case eq: Equal =>
        val compareInstruct = TypeChecker.getType(eq.rightOp, c) match {
          case ObjectType(_) | IntArrayType() => "if_acmpeq"
          case _ => "if_icmpeq"
        }
        asm(label) <++> gen(eq.leftOp, c) <++> gen(eq.rightOp, c) <++> genComparison(compareInstruct)
      case eq: NotEqual => // TODO: code duplication
        val compareInstruct = TypeChecker.getType(eq.rightOp, c) match {
          case ObjectType(_) | IntArrayType() => "if_acmpne"
          case _ => "if_icmpne"
        }
        asm(label) <++> gen(eq.leftOp, c) <++> gen(eq.rightOp, c) <++> genComparison(compareInstruct)
      case p: Parens =>
        asm(label) <++> gen(p.expr, c)
    }
}
