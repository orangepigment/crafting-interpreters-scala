package ru.orangepigment.lox.ast

import ru.orangepigment.lox.scanning.{Token, TokenLiteral}

sealed trait Expr

final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr

final case class Grouping(expression: Expr) extends Expr

final case class Unary(operator: Token, right: Expr) extends Expr

sealed trait Literal extends Expr

final case class NumberLiteral(literal: Double) extends Literal

final case class StringLiteral(literal: String) extends Literal

final case class IdentifierLiteral(literal: String) extends Literal

final case class BooleanLiteral(literal: Boolean) extends Literal

case object NilLiteral extends Literal




