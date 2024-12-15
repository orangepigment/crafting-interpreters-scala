package ru.orangepigment.lox.ast

sealed trait Stmt

sealed trait StmtWithExpression extends Stmt {
  def expression: Expr
}

final case class ExpressionStmt(expression: Expr) extends StmtWithExpression

final case class PrintStmt(expression: Expr) extends StmtWithExpression

final case class VarDeclStmt(name: IdentifierLiteral, expression: Option[Expr])
    extends Stmt
