package ru.orangepigment.lox.parser

import ru.orangepigment.lox.scanning.Token

final case class ParserError(token: Token, message: String)
