package ru.orangepigment.lox.ast

import ru.orangepigment.lox.scanning.{
  BinaryOp,
  IdentifierToken,
  Token,
  TokenLiteral,
  UnaryOp
}

sealed trait Expr

final case class Binary(left: Expr, operator: BinaryOp, right: Expr)
    extends Expr

final case class Grouping(expression: Expr) extends Expr

final case class Unary(operator: UnaryOp, right: Expr) extends Expr

sealed trait Literal extends Expr

sealed trait ValueLiteral[A] extends Literal {
  def literal: A
}

final case class NumberLiteral(override val literal: Double)
    extends ValueLiteral[Double]

final case class StringLiteral(override val literal: String)
    extends ValueLiteral[String]

final case class IdentifierLiteral(literal: IdentifierToken) extends Literal

final case class BooleanLiteral(override val literal: Boolean)
    extends ValueLiteral[Boolean]

case object NilLiteral extends Literal
