package ru.orangepigment.lox.scanning

case class Token(tpe: TokenType, lexeme: String, literal: Option[TokenLiteral], line: LineNum) {

  override def toString: String = s"L$line: $tpe $lexeme${literal.map(l => s" $l").getOrElse("")}"
}
