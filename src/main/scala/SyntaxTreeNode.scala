sealed trait SyntaxTreeNode {
  def index: Int
}

sealed trait Expr extends SyntaxTreeNode
case class IntLit(value: Long, index: Int) extends Expr
case class True(index: Int) extends Expr
case class False(index: Int) extends Expr
case class This(index: Int) extends Expr
case class Identifier(name: String, index: Int) extends Expr
case class NewArray(arraySize: Expr, index: Int) extends Expr
case class NewObject(typeName: Identifier, index: Int) extends Expr
case class Parens(expr: Expr, index: Int) extends Expr
case class ArrayLength(array: Expr, index: Int) extends Expr
case class MethodCall(obj: Expr, methodName: Identifier, args: Seq[Expr], index: Int) extends Expr
case class ArrayLookup(array: Expr, arrayIndex: Expr, index: Int) extends Expr
case class Not(expr: Expr, index: Int) extends Expr

sealed trait BinaryOp extends Expr {
  def leftOp: Expr
  def rightOp: Expr
}
case class Mult(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class Plus(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class Minus(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class LessThan(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class LessOrEqualThan(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class GreaterThan(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class GreaterOrEqualThan(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class Equal(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class NotEqual(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class And(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp
case class Or(leftOp: Expr, rightOp: Expr, index: Int) extends BinaryOp

sealed trait TypeNode extends SyntaxTreeNode
case class BooleanTypeNode(index: Int) extends TypeNode
case class IntArrayTypeNode(index: Int) extends TypeNode
case class IntTypeNode(index: Int) extends TypeNode
case class ObjectTypeNode(name: String, index: Int) extends TypeNode

sealed trait Stmt extends SyntaxTreeNode
case class ArrayAssign(array: Identifier, arrayIndex: Expr, newValue: Expr, index: Int) extends Stmt
case class Assign(assignee: Identifier, newValue: Expr, index: Int) extends Stmt
case class Block(stmtList: Seq[Stmt], index: Int) extends Stmt
case class If(condition: Expr, thenStmt: Stmt, elseStmt: Stmt, index: Int) extends Stmt
case class IfWithoutElse(condition: Expr, thenStmt: Stmt, index: Int) extends Stmt
case class Syso(printee: Expr, index: Int) extends Stmt
case class While(condition: Expr, stmt: Stmt, index: Int) extends Stmt

sealed trait GenVarDecl extends SyntaxTreeNode {
  def typeName: TypeNode
  def name: Identifier
}
case class VarDecl(typeName: TypeNode, name: Identifier, index: Int) extends GenVarDecl
case class Formal(typeName: TypeNode, name: Identifier, index: Int) extends GenVarDecl

case class MethodDecl(typeName: TypeNode, name: Identifier, argList: Seq[Formal], varDeclList: Seq[VarDecl],
                      stmts: Seq[Stmt], returnVal: Expr, index: Int) extends SyntaxTreeNode
case class ClassDecl(name: Identifier, varDecls: Seq[VarDecl], methodDecls: Seq[MethodDecl], index: Int)
  extends SyntaxTreeNode
case class MainClass(name: Identifier, stdArgsName: Identifier, varDecls: Seq[VarDecl], stmts: Seq[Stmt], index: Int)
  extends SyntaxTreeNode
case class Program(mainClass: MainClass, classDecls: Seq[ClassDecl], index: Int) extends SyntaxTreeNode