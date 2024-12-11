package ru.orangepigment.lox.errors

import ru.orangepigment.lox.scanning.{LineNum, Token}

sealed trait LoxError

final case class ScannerError(line: LineNum, message: String) extends LoxError

final case class ParserError(token: Token, message: String) extends LoxError

final case class RuntimeError(token: Token, message: String) extends LoxError