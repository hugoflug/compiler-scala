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
case class MethodCall(objectName: Expr, methodName: Identifier, args: Seq[Expr]) extends Expr
case class ArrayLookup(array: Expr, index: Expr) extends Expr
case class Not(expr: Expr) extends Expr
case class Mult(leftOp: Expr, rightOp: Expr) extends Expr
case class Plus(leftOp: Expr, rightOp: Expr) extends Expr
case class Minus(leftOp: Expr, rightOp: Expr) extends Expr
case class LessThan(leftOp: Expr, rightOp: Expr) extends Expr
case class LessOrEqualThan(leftOp: Expr, rightOp: Expr) extends Expr
case class GreaterThan(leftOp: Expr, rightOp: Expr) extends Expr
case class GreaterOrEqualThan(leftOp: Expr, rightOp: Expr) extends Expr
case class Equal(leftOp: Expr, rightOp: Expr) extends Expr
case class NotEqual(leftOp: Expr, rightOp: Expr) extends Expr
case class And(leftOp: Expr, rightOp: Expr) extends Expr
case class Or(leftOp: Expr, rightOp: Expr) extends Expr

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