package ru.orangepigment.lox.scanning

sealed trait TokenLiteral

final case class StringLiteral(raw: String) extends TokenLiteral

final case class NumberLiteral(raw: Double) extends TokenLiteral

final case class IdentifierLiteral(raw: String) extends TokenLiteral