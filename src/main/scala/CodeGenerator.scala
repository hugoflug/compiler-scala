import SymbolTableCreator.SymbolTable
import TypeChecker.Context

object CodeGenerator {
  def generate(program: Program, symTable: SymbolTable): String = ???

  private def esc(s: String) = "'" + s + "'"

  private def asm(s: String*) = s.mkString("\n") + "\n"

  private def genBinaryOp(binOp: BinaryOp, instruction: String, c: Context) =
    asm(gen(binOp.leftOp, c), gen(binOp.rightOp, c), instruction)

  private def gen(expr: Expr, c: Context): String = {
    expr match {
      case p: Plus =>
        genBinaryOp(p, "iadd", c)
      case m: Minus =>
        genBinaryOp(m, "isub", c)
      case gt: GreaterThan =>
        genBinaryOp(gt, "if_icmpgt", c)
      case get: GreaterOrEqualThan =>
        genBinaryOp(get, "if_icmpge", c)
      case lt: LessThan =>
        genBinaryOp(lt, "if_icmplt", c)
      case let: LessOrEqualThan =>
        genBinaryOp(let, "if_icmple", c)
      case m: Mult =>
        genBinaryOp(m, "imul", c)
      case i: IntLit =>
        asm("ldc ", i.value.toString)
      case n: Not =>
        asm(gen(n.expr, c), "iconst_1", "ixor")
      case n: NewArray =>
        asm(gen(n.arraySize, c), "newarray int")
      case n: NewObject =>
        val typeName = n.typeName.name
        asm("new " + esc(n.typeName.name), "dup", "invokespecial " + esc(typeName + "/<init>()V"))
      case a: ArrayLength =>
        asm(gen(a.array, c), "arraylength")
      case a: ArrayLookup =>
        asm(gen(a.array, c), gen(a.index, c), "iaload")
      case _: False =>
        asm("iconst_0")
    }
  }

  // case a: ArrayAssign => asm(gen(a.array), gen(a.index), gen(a.newValue), "iastore")
}
