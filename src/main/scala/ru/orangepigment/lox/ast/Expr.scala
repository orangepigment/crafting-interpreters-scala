package ru.orangepigment.lox.ast

import ru.orangepigment.lox.scanning.TokenType.{FALSE, TRUE, NIL}
import ru.orangepigment.lox.scanning.{Token, TokenLiteral}

sealed trait Expr

final case class Binary(left: Expr, operator: Token, right: Expr) extends Expr

final case class Grouping(expression: Expr) extends Expr

final case class Literal(literal: TokenLiteral) extends Expr

final case class Keyword(word: TRUE.type | FALSE.type | NIL.type) extends Expr

final case class Unary(operator: Token, right: Expr) extends Expr




