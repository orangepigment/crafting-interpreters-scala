package ru.orangepigment.lox.scanning

sealed trait Token {
  val lexeme: String
  val line: LineNum

  override def toString: String = s"L$line: ${getClass.getSimpleName} $lexeme"
}

sealed trait UnaryOp extends Token
sealed trait BinaryOp extends Token

// Single-character tokens.
final case class LeftParen(
  override val lexeme: String,
  override val line: LineNum
) extends Token

final case class RightParen(
  override val lexeme: String,
  override val line: LineNum
) extends Token

final case class LeftBrace(
  override val lexeme: String,
  override val line: LineNum
) extends Token

final case class RightBrace(
  override val lexeme: String,
  override val line: LineNum
) extends Token

final case class Comma(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Dot(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Minus(override val lexeme: String, override val line: LineNum)
    extends UnaryOp
    with BinaryOp

final case class Plus(override val lexeme: String, override val line: LineNum)
    extends BinaryOp

final case class Semicolon(
  override val lexeme: String,
  override val line: LineNum
) extends Token

final case class Slash(override val lexeme: String, override val line: LineNum)
    extends BinaryOp

final case class Star(override val lexeme: String, override val line: LineNum)
    extends BinaryOp

// One or two character tokens.
final case class Bang(override val lexeme: String, override val line: LineNum)
    extends UnaryOp

final case class BangEqual(
  override val lexeme: String,
  override val line: LineNum
) extends BinaryOp

final case class Equal(override val lexeme: String, override val line: LineNum)
    extends Token

final case class EqualEqual(
  override val lexeme: String,
  override val line: LineNum
) extends BinaryOp

final case class Greater(
  override val lexeme: String,
  override val line: LineNum
) extends BinaryOp

final case class GreaterEqual(
  override val lexeme: String,
  override val line: LineNum
) extends BinaryOp

final case class Less(override val lexeme: String, override val line: LineNum)
    extends BinaryOp

final case class LessEqual(
  override val lexeme: String,
  override val line: LineNum
) extends BinaryOp

// Literals.
sealed trait TokenLiteral[A] extends Token {
  val raw: A

  override def toString: String =
    s"L$line: ${getClass.getSimpleName} $lexeme $raw"
}

final case class Identifier(
  override val lexeme: String,
  override val raw: String,
  override val line: LineNum
) extends TokenLiteral[String]

final case class StringToken(
  override val lexeme: String,
  override val raw: String,
  override val line: LineNum
) extends TokenLiteral[String]

final case class Number(
  override val lexeme: String,
  override val raw: Double,
  override val line: LineNum
) extends TokenLiteral[Double]

// Keywords

final case class And(override val lexeme: String, override val line: LineNum)
    extends Token //BinaryOp

final case class Class(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Else(override val lexeme: String, override val line: LineNum)
    extends Token

final case class False(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Fun(override val lexeme: String, override val line: LineNum)
    extends Token

final case class For(override val lexeme: String, override val line: LineNum)
    extends Token

final case class If(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Nil(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Or(override val lexeme: String, override val line: LineNum)
    extends Token //BinaryOp

final case class Print(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Return(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Super(override val lexeme: String, override val line: LineNum)
    extends Token

final case class This(override val lexeme: String, override val line: LineNum)
    extends Token

final case class True(override val lexeme: String, override val line: LineNum)
    extends Token

final case class Var(override val lexeme: String, override val line: LineNum)
    extends Token

final case class While(override val lexeme: String, override val line: LineNum)
    extends Token

final case class EOF(override val lexeme: String, override val line: LineNum)
    extends Token

object TokenType {
  type BuildToken = (String, LineNum) => Token

  val oneCharTokens: Map[Char, BuildToken] = Map(
    '(' -> LeftParen.apply,
    ')' -> RightParen.apply,
    '{' -> LeftBrace.apply,
    '}' -> RightBrace.apply,
    ',' -> Comma.apply,
    '.' -> Dot.apply,
    '-' -> Minus.apply,
    '+' -> Plus.apply,
    ';' -> Semicolon.apply,
    '*' -> Star.apply
  )

  val oneOrTwoCharTokens: Map[Char, (Char, BuildToken, BuildToken)] = Map(
    '!' -> ('=', Bang.apply, BangEqual.apply),
    '=' -> ('=', Equal.apply, EqualEqual.apply),
    '<' -> ('=', Less.apply, LessEqual.apply),
    '>' -> ('=', Greater.apply, GreaterEqual.apply)
  )

  val keywords: Map[String, BuildToken] = Map(
    "and" -> And.apply,
    "class" -> Class.apply,
    "else" -> Else.apply,
    "false" -> False.apply,
    "for" -> For.apply,
    "fun" -> Fun.apply,
    "if" -> If.apply,
    "nil" -> Nil.apply,
    "or" -> Or.apply,
    "print" -> Print.apply,
    "return" -> Return.apply,
    "super" -> Super.apply,
    "this" -> This.apply,
    "true" -> True.apply,
    "var" -> Var.apply,
    "while" -> While.apply
  )

}
