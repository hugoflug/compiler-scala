import SymbolTableCreator.{SymbolTable, Var}
import TypeChecker.Context

object CodeGenerator {
  case class CodegenResult(program: String, currentLabel: Int) {
    def flatMap(f: Int => CodegenResult): CodegenResult = {
      val result = f(currentLabel)
      CodegenResult(program + result.program, result.currentLabel)
    }

    def map(f: Int => Int): CodegenResult =
      CodegenResult(program, f(currentLabel))

    def <++>(f: Int => CodegenResult): CodegenResult = flatMap(f)

    def <+>(f: Int => CodegenResult): CodegenResult = flatMap(f)
    def <+>(f: Int => String): CodegenResult = {
      val result = f(currentLabel)
      CodegenResult(program + result, currentLabel)
    }

    def <+>(s: String): CodegenResult = CodegenResult(program + s + "\n", currentLabel)
  }

  def asm(currentLabel: Int): CodegenResult = CodegenResult("", currentLabel)

  def generate(program: Program, symTable: SymbolTable): String = ???

  private def esc(s: String) = "'" + s + "'"

  private def genBinaryOp(binOp: BinaryOp, instruction: String, c: Context)(label: Int) =
    asm(label) <+> gen(binOp.leftOp, c) <+> gen(binOp.rightOp, c) <+> instruction

  private def genShortCircuitOp(binOp: BinaryOp, compInstruction: String, c: Context)(label: Int) =
    asm(label) <+>
    gen(binOp.leftOp, c) <+>
    "dup" <++>
    (label =>
      asm(label + 1) <+>
      compInstruction + " " + label <+>
      "pop" <+>
      gen(binOp.rightOp, c) <+>
      "end: " + label)

  private def oneOf[T](options: Option[T]*): Option[T] =
    if (options.isEmpty) None
    else options.head.orElse(oneOf(options.tail:_*))

  private def toTypeDescriptor(t: Type): String = {
      case ObjectType(name) => "L" + name + ";"
      case IntType() => "I"
      case IntArrayType() => "[I"
      case BooleanType() => "I"
    }

  private def genAll(nodes: Seq[SyntaxTreeNode], c: Context)(label: Int): CodegenResult =
    nodes.map(gen(_, c)).foldLeft(asm(label))(_ <++> _)

  private def gen(node: SyntaxTreeNode, c: Context)(label: Int): CodegenResult =
    node match {
      case a: ArrayAssign =>
        asm(label) <+> gen(a.array, c) <+> gen(a.index, c) <+> gen(a.newValue, c) <+> "iastore"
      case a: Assign =>
        val assignee = a.assignee.name
        val method = c.currentMethod.get
        val methodVars = oneOf(method.locals.get(assignee), method.params.get(assignee))
        methodVars match {
          case Some(_) => {
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
            val typeDesc = toTypeDescriptor(type_)
            asm(label) <+> "aload_0" <+> "swap" <+> "putfield " + esc(fieldDesc) + " " + esc(typeDesc)
        }
      case b: Block =>
        asm(label) <+> genAll(b.stmtList, c)
      case cd: ClassDecl => {
        var newContext = c.copy(currentClass = c.symTable.get(cd.name.name))
        ???
      }

    }

  private def gen(expr: Expr, c: Context)(label: Int): CodegenResult =
    expr match {
      case p: Plus =>
        asm(label) <+> genBinaryOp(p, "iadd", c)
      case m: Minus =>
        asm(label) <+> genBinaryOp(m, "isub", c)
      case gt: GreaterThan =>
        asm(label) <+> genBinaryOp(gt, "if_icmpgt", c)
      case get: GreaterOrEqualThan =>
        asm(label) <+> genBinaryOp(get, "if_icmpge", c)
      case lt: LessThan =>
        asm(label) <+> genBinaryOp(lt, "if_icmplt", c)
      case let: LessOrEqualThan =>
        asm(label) <+> genBinaryOp(let, "if_icmple", c)
      case m: Mult =>
        asm(label) <+> genBinaryOp(m, "imul", c)
      case i: IntLit =>
        asm(label) <+> "ldc " <+> i.value.toString
      case n: Not =>
        asm(label) <+> gen(n.expr, c) <+> "iconst_1" <+> "ixor"
      case n: NewArray =>
        asm(label) <+> gen(n.arraySize, c) <+> "newarray int"
      case n: NewObject =>
        val typeName = n.typeName.name
        asm(label) <+> "new " + esc(n.typeName.name) <+> "dup" <+> "invokespecial " + esc(typeName + "/<init>()V")
      case a: ArrayLength =>
        asm(label) <+> gen(a.array, c) <+> "arraylength"
      case a: ArrayLookup =>
        asm(label) <+> gen(a.array, c) <+> gen(a.index, c) <+> "iaload"
      case _: False =>
        asm(label) <+> "iconst_0"
      case or: Or =>
        asm(label) <+> genShortCircuitOp(or, "ifne", c)
      case and: And =>
        asm(label) <+> genShortCircuitOp(and, "ifeq", c)
    }

  // case a: ArrayAssign => asm(gen(a.array), gen(a.index), gen(a.newValue), "iastore")
}
