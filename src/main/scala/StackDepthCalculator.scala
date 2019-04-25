object StackDepthCalculator {

  def maxStackDepth(syntaxTreeNode: SyntaxTreeNode) =
    maxDepth(syntaxTreeNode).apply(DepthInfo(0, 0)).max

  def maxStackDepth(nodes: Seq[SyntaxTreeNode]) =
    maxDepth(nodes).apply(DepthInfo(0, 0)).max

  private class RichFunction1[T, R](i: T => R) {
    def |>[A](f: R => A) = i.andThen(f)
  }
  private implicit def richFunction1[T, R](i: T => R): RichFunction1[T, R] = new RichFunction1(i)

  private case class DepthInfo(current: Int, max: Int)

  private def changeStackDepth(delta: Int)(depthInfo: DepthInfo): DepthInfo = {
    val newCurrent = depthInfo.current + delta
    val newMax = if (newCurrent > depthInfo.max) newCurrent else depthInfo.max

    DepthInfo(newCurrent, newMax)
  }

  private def maxDepth(nodes: Seq[SyntaxTreeNode]) : DepthInfo => DepthInfo =
    nodes.foldLeft((a: DepthInfo) => a) { (a, b) => a |> maxDepth(b) }

  private def maxDepth(syntaxTreeNode: SyntaxTreeNode): DepthInfo => DepthInfo = syntaxTreeNode match {
      case And(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        changeStackDepth(+1) |>
        changeStackDepth(-2) |>
        maxDepth(rightOp)

      case ArrayAssign(id, arrayIndex, newValue, _) =>
        maxDepth(id) |>
        maxDepth(arrayIndex) |>
        maxDepth(newValue) |>
        changeStackDepth(-3)

      case ArrayLength(array, _) =>
        maxDepth(array)

      case ArrayLookup(array, arrayIndex, _) =>
        maxDepth(array) |>
        maxDepth(arrayIndex) |>
        changeStackDepth(-1)

      case Assign(_, newValue, _) =>
        maxDepth(newValue) |>
        changeStackDepth(+1) |>
        changeStackDepth(-2)

      case Block(stmts, _) =>
        maxDepth(stmts)

      case Equal(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-2) |>
        changeStackDepth(+1)

      case False(_) =>
        changeStackDepth(+1)

      case Identifier(_, _) =>
        changeStackDepth(+1)

      case If(cond, thenStmt, elseStmt, _) =>
        maxDepth(cond) |>
        changeStackDepth(-1) |>
        maxDepth(thenStmt) |>
        maxDepth(elseStmt)

      case IfWithoutElse(cond, thenStmt, _) =>
        maxDepth(cond) |>
        changeStackDepth(-1) |>
        maxDepth(thenStmt)

      case IntLit(_, _) =>
        changeStackDepth(+1)

      case LessThan(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-2) |>
        changeStackDepth(+1)

      case LessOrEqualThan(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-2) |>
        changeStackDepth(+1)

      case MethodCall(obj, _, args, _) =>
        maxDepth(obj) |>
        maxDepth(args) |>
        changeStackDepth(-1 - args.length) |>
        changeStackDepth(+1)

      case MethodDecl(_, _, _, _, stmts, returnVal, _) =>
        maxDepth(stmts) |>
        maxDepth(returnVal) |>
        changeStackDepth(-1)

      case Minus(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-1)

      case GreaterThan(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-2) |>
        changeStackDepth(+1)

      case GreaterOrEqualThan(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-2) |>
        changeStackDepth(+1)

      case Mult(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-1)

      case NewObject(_, _) =>
        changeStackDepth(+2) _ |>
        changeStackDepth(-1)

      case NewArray(arraySize, _) =>
        maxDepth(arraySize)

      case Not(expr, _) =>
        maxDepth(expr) |>
        changeStackDepth(+1) |>
        changeStackDepth(-1)

      case NotEqual(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-2) |>
        changeStackDepth(+1)

      case Or(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        changeStackDepth(+1) |>
        changeStackDepth(-2) |>
        maxDepth(rightOp)

      case Parens(expr, _) =>
        maxDepth(expr)

      case Plus(leftOp, rightOp, _) =>
        maxDepth(leftOp) |>
        maxDepth(rightOp) |>
        changeStackDepth(-1)

      case Syso(printee, _) =>
        changeStackDepth(+1) _ |>
        maxDepth(printee) |>
        changeStackDepth(-2)

      case This(_) =>
        changeStackDepth(+1)

      case True(_) =>
        changeStackDepth(+1)

      case While(cond, stmt, _) =>
        maxDepth(cond) |>
        changeStackDepth(-1) |>
        maxDepth(stmt)

      case _ =>
        changeStackDepth(0)
    }
}
