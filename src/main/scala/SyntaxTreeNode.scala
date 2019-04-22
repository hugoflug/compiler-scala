abstract class SyntaxTreeNode

abstract class Expr extends SyntaxTreeNode
case class IntLit(value: Int) extends Expr
case class True() extends Expr
case class False() extends Expr
case class This() extends Expr
case class Identifier(name: String) extends Expr
case class NewArray(arraySize: Expr) extends Expr
case class NewObject(typeName: Identifier) extends Expr
case class Parens(expr: Expr) extends Expr
case class ArrayLength(array: Expr) extends Expr
case class MethodCall(obj: Expr, methodName: Identifier, args: Seq[Expr]) extends Expr
case class ArrayLookup(array: Expr, index: Expr) extends Expr
case class Not(expr: Expr) extends Expr

abstract class BinaryOp(val leftOp: Expr, val rightOp: Expr) extends Expr
case class Mult(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class Plus(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class Minus(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class LessThan(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class LessOrEqualThan(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class GreaterThan(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class GreaterOrEqualThan(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class Equal(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class NotEqual(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class And(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)
case class Or(override val leftOp: Expr, override val rightOp: Expr) extends BinaryOp(leftOp, rightOp)

abstract class Type extends SyntaxTreeNode
case class BooleanType() extends Type
case class IntArrayType() extends Type
case class IntType() extends Type
case class ObjectType(name: String) extends Type
case class Void() extends Type

abstract class Stmt extends SyntaxTreeNode
case class ArrayAssign(array: Identifier, index: Expr, newValue: Expr) extends Stmt
case class Assign(assignee: Identifier, newValue: Expr) extends Stmt
case class Block(stmtList: Seq[Stmt]) extends Stmt
case class If(condition: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt
case class IfWithoutElse(condition: Expr, thenStmt: Stmt) extends Stmt
case class Syso(printee: Expr) extends Stmt
case class While(condition: Expr, stmt: Stmt) extends Stmt

case class VarDecl(typeName: Type, name: Identifier) extends SyntaxTreeNode
case class Formal(typeName: Type, name: Identifier) extends SyntaxTreeNode
case class MethodDecl(typeName: Type, name: Identifier, argList: Seq[Formal], varDeclList: Seq[VarDecl],
                      stmts: Seq[Stmt], returnVal: Expr) extends SyntaxTreeNode
case class ClassDecl(name: Identifier, varDecls: Seq[VarDecl], methodDecls: Seq[MethodDecl]) extends SyntaxTreeNode
case class MainClass(name: Identifier, stdArgsName: Identifier, varDecls: Seq[VarDecl], stmts: Seq[Stmt]) extends SyntaxTreeNode
case class Program(mainClass: MainClass, classDecls: Seq[ClassDecl]) extends SyntaxTreeNode