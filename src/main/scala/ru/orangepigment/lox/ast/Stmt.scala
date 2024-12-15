package ru.orangepigment.lox.ast

import ru.orangepigment.lox.scanning.IdentifierToken

sealed trait Stmt

sealed trait StmtWithExpression extends Stmt {
  def expression: Expr
}

final case class ExpressionStmt(expression: Expr) extends StmtWithExpression

final case class PrintStmt(expression: Expr) extends StmtWithExpression

final case class VarDeclStmt(name: IdentifierToken, expression: Option[Expr])
    extends Stmt
