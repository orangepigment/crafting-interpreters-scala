package ru.orangepigment.lox.scanning

import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.scanning.TokenType._

class ScannerSpec extends AnyFlatSpec with Matchers with Inspectors {

  private def line(i: Int): LineNum = LineNum(i);

  private def eofToken(i: Int = 1) = Token(EOF, "", None, line(i))

  "Scanner.scanTokens" should "scan fixed length tokens correctly" in {
    Scanner.scanTokens("(") shouldEqual Right(List(Token(LEFT_PAREN, "(", None, line(1)), eofToken()))
    Scanner.scanTokens(")") shouldEqual Right(List(Token(RIGHT_PAREN, ")", None, line(1)), eofToken()))

    Scanner.scanTokens("{") shouldEqual Right(List(Token(LEFT_BRACE, "{", None, line(1)), eofToken()))
    Scanner.scanTokens("}") shouldEqual Right(List(Token(RIGHT_BRACE, "}", None, line(1)), eofToken()))

    Scanner.scanTokens(",") shouldEqual Right(List(Token(COMMA, ",", None, line(1)), eofToken()))
    Scanner.scanTokens(".") shouldEqual Right(List(Token(DOT, ".", None, line(1)), eofToken()))

    Scanner.scanTokens("-") shouldEqual Right(List(Token(MINUS, "-", None, line(1)), eofToken()))
    Scanner.scanTokens("+") shouldEqual Right(List(Token(PLUS, "+", None, line(1)), eofToken()))

    Scanner.scanTokens(";") shouldEqual Right(List(Token(SEMICOLON, ";", None, line(1)), eofToken()))

    Scanner.scanTokens("/") shouldEqual Right(List(Token(SLASH, "/", None, line(1)), eofToken()))
    Scanner.scanTokens("*") shouldEqual Right(List(Token(STAR, "*", None, line(1)), eofToken()))

    Scanner.scanTokens("!") shouldEqual Right(List(Token(BANG, "!", None, line(1)), eofToken()))
    Scanner.scanTokens("!=") shouldEqual Right(List(Token(BANG_EQUAL, "!=", None, line(1)), eofToken()))
    Scanner.scanTokens("=") shouldEqual Right(List(Token(EQUAL, "=", None, line(1)), eofToken()))
    Scanner.scanTokens("==") shouldEqual Right(List(Token(EQUAL_EQUAL, "==", None, line(1)), eofToken()))
    Scanner.scanTokens("<") shouldEqual Right(List(Token(LESS, "<", None, line(1)), eofToken()))
    Scanner.scanTokens("<=") shouldEqual Right(List(Token(LESS_EQUAL, "<=", None, line(1)), eofToken()))
    Scanner.scanTokens(">") shouldEqual Right(List(Token(GREATER, ">", None, line(1)), eofToken()))
    Scanner.scanTokens(">=") shouldEqual Right(List(Token(GREATER_EQUAL, ">=", None, line(1)), eofToken()))
  }

  it should "ignore one-line comments" in {
    Scanner.scanTokens("// Comment") shouldEqual Right(List(eofToken()))

    Scanner.scanTokens(
      """// Comment
        |/""".stripMargin) shouldEqual Right(List(Token(SLASH, "/", None, line(2)), eofToken(2)))

    Scanner.scanTokens(
      """/
        |// Comment""".stripMargin) shouldEqual Right(List(Token(SLASH, "/", None, line(1)), eofToken(2)))

    Scanner.scanTokens(
      """/
        |// Comment
        |/""".stripMargin) shouldEqual Right(List(Token(SLASH, "/", None, line(1)), Token(SLASH, "/", None, line(3)), eofToken(3)))
  }

  it should "ignore multi-line comments" in {
    Scanner.scanTokens(
      """/* Long
        |long *
        |comment */
        |fun F""".stripMargin) shouldEqual Right(List(
      Token(FUN, "fun", None, line(4)),
      Token(IDENTIFIER, "F", Some(IdentifierLiteral("F")), line(4)),
      eofToken(4)
    ))
  }

  it should "return error on unterminated multi-line comments" in {
    Scanner.scanTokens(
      """/* Unfinished
        |long
        |comme""".stripMargin) shouldEqual Left(ScannerError(line(3), "Unterminated multi-line comment."))
  }

  it should "scan int and decimal digits" in {
    Scanner.scanTokens("1234") shouldEqual Right(List(Token(NUMBER, "1234", Some(NumberLiteral(1234.0)), line(1)), eofToken()))
    Scanner.scanTokens("1234.5") shouldEqual Right(List(Token(NUMBER, "1234.5", Some(NumberLiteral(1234.5)), line(1)), eofToken()))
    Scanner.scanTokens("0.5") shouldEqual Right(List(Token(NUMBER, "0.5", Some(NumberLiteral(0.5)), line(1)), eofToken()))

    Scanner.scanTokens("1.") shouldEqual Right(List(
      Token(NUMBER, "1", Some(NumberLiteral(1)), line(1)),
      Token(DOT, ".", None, line(1)),
      eofToken()
    ))

    Scanner.scanTokens(".1") shouldEqual Right(List(
      Token(DOT, ".", None, line(1)),
      Token(NUMBER, "1", Some(NumberLiteral(1)), line(1)),
      eofToken()
    ))
  }

  it should "scan keywords" in {
    Scanner.scanTokens("and") shouldEqual Right(List(Token(AND, "and", None, line(1)), eofToken()))
    Scanner.scanTokens("class") shouldEqual Right(List(Token(CLASS, "class", None, line(1)), eofToken()))
    Scanner.scanTokens("else") shouldEqual Right(List(Token(ELSE, "else", None, line(1)), eofToken()))
    Scanner.scanTokens("false") shouldEqual Right(List(Token(FALSE, "false", None, line(1)), eofToken()))
    Scanner.scanTokens("false") shouldEqual Right(List(Token(FALSE, "false", None, line(1)), eofToken()))
    Scanner.scanTokens("fun") shouldEqual Right(List(Token(FUN, "fun", None, line(1)), eofToken()))
    Scanner.scanTokens("for") shouldEqual Right(List(Token(FOR, "for", None, line(1)), eofToken()))
    Scanner.scanTokens("if") shouldEqual Right(List(Token(IF, "if", None, line(1)), eofToken()))
    Scanner.scanTokens("nil") shouldEqual Right(List(Token(NIL, "nil", None, line(1)), eofToken()))
    Scanner.scanTokens("or") shouldEqual Right(List(Token(OR, "or", None, line(1)), eofToken()))
    Scanner.scanTokens("print") shouldEqual Right(List(Token(PRINT, "print", None, line(1)), eofToken()))
    Scanner.scanTokens("return") shouldEqual Right(List(Token(RETURN, "return", None, line(1)), eofToken()))
    Scanner.scanTokens("super") shouldEqual Right(List(Token(SUPER, "super", None, line(1)), eofToken()))
    Scanner.scanTokens("this") shouldEqual Right(List(Token(THIS, "this", None, line(1)), eofToken()))
    Scanner.scanTokens("true") shouldEqual Right(List(Token(TRUE, "true", None, line(1)), eofToken()))
    Scanner.scanTokens("var") shouldEqual Right(List(Token(VAR, "var", None, line(1)), eofToken()))
    Scanner.scanTokens("while") shouldEqual Right(List(Token(WHILE, "while", None, line(1)), eofToken()))
  }

  it should "scan one-line strings" in {
    Scanner.scanTokens("\"some text\"") shouldEqual Right(List(Token(STRING, "\"some text\"", Some(StringLiteral("some text")), line(1)), eofToken()))
  }

  it should "scan multi-line strings" in {
    val expectedLiteralValue =
      """really
        |long
        |text""".stripMargin

    val tokens =
      Scanner.scanTokens(
        """"really
          |long
          |text"""".stripMargin) shouldEqual Right(List(
        Token(
          STRING,
          """"really
            |long
            |text"""".stripMargin,
          Some(StringLiteral(expectedLiteralValue)), line(1)), eofToken(3)
      ))
  }

  it should "return error on unterminated strings" in {
    Scanner.scanTokens("\"unfinished stri") shouldEqual Left(ScannerError(line(1), "Unterminated string."))

    Scanner.scanTokens(
      """"long
        |unfinished stri""".stripMargin) shouldEqual Left(ScannerError(line(2), "Unterminated string."))
  }

  it should "scan identifiers" in {
    Scanner.scanTokens("a") shouldEqual Right(List(Token(IDENTIFIER, "a", Some(IdentifierLiteral("a")), line(1)), eofToken()))
    Scanner.scanTokens("a_") shouldEqual Right(List(Token(IDENTIFIER, "a_", Some(IdentifierLiteral("a_")), line(1)), eofToken()))
    Scanner.scanTokens("ab") shouldEqual Right(List(Token(IDENTIFIER, "ab", Some(IdentifierLiteral("ab")), line(1)), eofToken()))
    Scanner.scanTokens("a_b") shouldEqual Right(List(Token(IDENTIFIER, "a_b", Some(IdentifierLiteral("a_b")), line(1)), eofToken()))
    Scanner.scanTokens("ab2_") shouldEqual Right(List(Token(IDENTIFIER, "ab2_", Some(IdentifierLiteral("ab2_")), line(1)), eofToken()))
  }

  it should "scan empty input" in {
    Scanner.scanTokens("") shouldEqual Right(List(eofToken()))
  }

  it should "ignore whitespace and handle linebreak" in {
    Scanner.scanTokens(" \t ") shouldEqual Right(List(eofToken()))
    Scanner.scanTokens("\r\t\n   ") shouldEqual Right(List(eofToken(2)))
    Scanner.scanTokens("\n\n") shouldEqual Right(List(eofToken(3)))
  }

  it should "scan multi-token input" in {
    val source =
      """// this is a comment
        |(( )){} // grouping stuff
        |!*+-/=<> <= == // operators
        |a.b + "str" * 2.3""".stripMargin

    Scanner.scanTokens(source) shouldEqual Right(List(
      Token(LEFT_PAREN, "(", None, line(2)),
      Token(LEFT_PAREN, "(", None, line(2)),
      Token(RIGHT_PAREN, ")", None, line(2)),
      Token(RIGHT_PAREN, ")", None, line(2)),
      Token(LEFT_BRACE, "{", None, line(2)),
      Token(RIGHT_BRACE, "}", None, line(2)),

      Token(BANG, "!", None, line(3)),
      Token(STAR, "*", None, line(3)),
      Token(PLUS, "+", None, line(3)),
      Token(MINUS, "-", None, line(3)),
      Token(SLASH, "/", None, line(3)),
      Token(EQUAL, "=", None, line(3)),
      Token(LESS, "<", None, line(3)),
      Token(GREATER, ">", None, line(3)),
      Token(LESS_EQUAL, "<=", None, line(3)),
      Token(EQUAL_EQUAL, "==", None, line(3)),

      Token(IDENTIFIER, "a", Some(IdentifierLiteral("a")), line(4)),
      Token(DOT, ".", None, line(4)),
      Token(IDENTIFIER, "b", Some(IdentifierLiteral("b")), line(4)),
      Token(PLUS, "+", None, line(4)),
      Token(STRING, "\"str\"", Some(StringLiteral("str")), line(4)),
      Token(STAR, "*", None, line(4)),
      Token(NUMBER, "2.3", Some(NumberLiteral(2.3)), line(4)),

      eofToken(4)
    ))
  }

  it should "return error on unexpected symbols" in {
    Scanner.scanTokens("A + ~b") shouldEqual Left(ScannerError(line(1), "Unexpected character '~'."))
  }
}
