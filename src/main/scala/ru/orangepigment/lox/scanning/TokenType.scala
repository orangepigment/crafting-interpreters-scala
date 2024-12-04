package ru.orangepigment.lox.scanning

enum TokenType {
  // Single-character tokens.
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  EOF
}

object TokenType {

  val oneCharTokens: Map[Char, TokenType] = Map(
    '(' -> LEFT_PAREN,
    ')' -> RIGHT_PAREN,
    '{' -> LEFT_BRACE,
    '}' -> RIGHT_BRACE,
    ',' -> COMMA,
    '.' -> DOT,
    '-' -> MINUS,
    '+' -> PLUS,
    ';' -> SEMICOLON,
    '*' -> STAR
  )

  val oneOrTwoCharTokens: Map[Char, (Char, TokenType, TokenType)] = Map(
    '!' -> ('=', BANG, BANG_EQUAL),
    '=' -> ('=', EQUAL, EQUAL_EQUAL),
    '<' -> ('=', LESS, LESS_EQUAL),
    '>' -> ('=', GREATER, GREATER_EQUAL)
  )

}
  