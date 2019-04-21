import SymbolTableCreator.SymbolTable

object CodeGenerator {
  def generate(program: Program, symTable: SymbolTable): String = ???
  private def genBinaryOp(binOp: BinaryOp, instruction: String) =
    asm(gen(binOp.leftOp), gen(binOp.rightOp), instruction)

  private def esc(s: String) = "'" + s + "'"

  private def asm(s: String*) = s.mkString("\n")

  private def gen(expr: Expr): String = {
    expr match {
      case p: Plus => genBinaryOp(p, "iadd")
      case m: Minus => genBinaryOp(m, "isub")
      case gt: GreaterThan => genBinaryOp(gt, "if_icmpgt")
      case get: GreaterOrEqualThan => genBinaryOp(get, "if_icmpge")
      case lt: LessThan => genBinaryOp(lt, "if_icmplt")
      case let: LessOrEqualThan => genBinaryOp(let, "if_icmple")
      case m: Mult => genBinaryOp(m, "imul")
      case i: IntLit => asm("ldc ", i.value.toString)
      case n: Not => asm(gen(n.expr), "iconst_1", "ixor")
      case n: NewArray => asm(gen(n.arraySize), "newarray int")
      case n: NewObject =>
        val typeName = n.typeName.name
        asm("new " + esc(n.typeName.name), "dup", "invokespecial " + esc(typeName + "/<init>()V"))

    }
  }
}
