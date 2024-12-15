package ru.orangepigment.lox.scanning

import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.errors.ScannerError

class ScannerSpec extends AnyFlatSpec with Matchers with Inspectors {

  private def line(i: Int): LineNum = LineNum(i);

  private def eofToken(i: Int = 1) = EOF("", line(i))

  "Scanner.scanTokens" should "scan fixed length tokens correctly" in {
    Scanner.scanTokens("(") shouldEqual Right(
      List(LeftParen("(", line(1)), eofToken())
    )
    Scanner.scanTokens(")") shouldEqual Right(
      List(RightParen(")", line(1)), eofToken())
    )

    Scanner.scanTokens("{") shouldEqual Right(
      List(LeftBrace("{", line(1)), eofToken())
    )
    Scanner.scanTokens("}") shouldEqual Right(
      List(RightBrace("}", line(1)), eofToken())
    )

    Scanner.scanTokens(",") shouldEqual Right(
      List(Comma(",", line(1)), eofToken())
    )
    Scanner.scanTokens(".") shouldEqual Right(
      List(Dot(".", line(1)), eofToken())
    )

    Scanner.scanTokens("-") shouldEqual Right(
      List(Minus("-", line(1)), eofToken())
    )
    Scanner.scanTokens("+") shouldEqual Right(
      List(Plus("+", line(1)), eofToken())
    )

    Scanner.scanTokens(";") shouldEqual Right(
      List(Semicolon(";", line(1)), eofToken())
    )

    Scanner.scanTokens("/") shouldEqual Right(
      List(Slash("/", line(1)), eofToken())
    )
    Scanner.scanTokens("*") shouldEqual Right(
      List(Star("*", line(1)), eofToken())
    )

    Scanner.scanTokens("!") shouldEqual Right(
      List(Bang("!", line(1)), eofToken())
    )
    Scanner.scanTokens("!=") shouldEqual Right(
      List(BangEqual("!=", line(1)), eofToken())
    )
    Scanner.scanTokens("=") shouldEqual Right(
      List(Equal("=", line(1)), eofToken())
    )
    Scanner.scanTokens("==") shouldEqual Right(
      List(EqualEqual("==", line(1)), eofToken())
    )
    Scanner.scanTokens("<") shouldEqual Right(
      List(Less("<", line(1)), eofToken())
    )
    Scanner.scanTokens("<=") shouldEqual Right(
      List(LessEqual("<=", line(1)), eofToken())
    )
    Scanner.scanTokens(">") shouldEqual Right(
      List(Greater(">", line(1)), eofToken())
    )
    Scanner.scanTokens(">=") shouldEqual Right(
      List(GreaterEqual(">=", line(1)), eofToken())
    )
  }

  it should "ignore one-line comments" in {
    Scanner.scanTokens("// Comment") shouldEqual Right(List(eofToken()))

    Scanner.scanTokens("""// Comment
        |/""".stripMargin) shouldEqual Right(
      List(Slash("/", line(2)), eofToken(2))
    )

    Scanner.scanTokens("""/
        |// Comment""".stripMargin) shouldEqual Right(
      List(Slash("/", line(1)), eofToken(2))
    )

    Scanner.scanTokens("""/
        |// Comment
        |/""".stripMargin) shouldEqual Right(
      List(Slash("/", line(1)), Slash("/", line(3)), eofToken(3))
    )
  }

  it should "ignore multi-line comments" in {
    Scanner.scanTokens("""/* Long
        |long *
        |comment */
        |fun F""".stripMargin) shouldEqual Right(
      List(Fun("fun", line(4)), IdentifierToken("F", "F", line(4)), eofToken(4))
    )
  }

  it should "return error on unterminated multi-line comments" in {
    Scanner.scanTokens("""/* Unfinished
        |long
        |comme""".stripMargin) shouldEqual Left(
      ScannerError(line(3), "Unterminated multi-line comment.")
    )
  }

  it should "scan int and decimal digits" in {
    Scanner.scanTokens("1234") shouldEqual Right(
      List(NumberToken("1234", 1234.0, line(1)), eofToken())
    )
    Scanner.scanTokens("1234.5") shouldEqual Right(
      List(NumberToken("1234.5", 1234.5, line(1)), eofToken())
    )
    Scanner.scanTokens("0.5") shouldEqual Right(
      List(NumberToken("0.5", 0.5, line(1)), eofToken())
    )

    Scanner.scanTokens("1.") shouldEqual Right(
      List(NumberToken("1", 1, line(1)), Dot(".", line(1)), eofToken())
    )

    Scanner.scanTokens(".1") shouldEqual Right(
      List(Dot(".", line(1)), NumberToken("1", 1, line(1)), eofToken())
    )
  }

  it should "scan keywords" in {
    Scanner.scanTokens("and") shouldEqual Right(
      List(And("and", line(1)), eofToken())
    )
    Scanner.scanTokens("class") shouldEqual Right(
      List(Class("class", line(1)), eofToken())
    )
    Scanner.scanTokens("else") shouldEqual Right(
      List(Else("else", line(1)), eofToken())
    )
    Scanner.scanTokens("false") shouldEqual Right(
      List(False("false", line(1)), eofToken())
    )
    Scanner.scanTokens("fun") shouldEqual Right(
      List(Fun("fun", line(1)), eofToken())
    )
    Scanner.scanTokens("for") shouldEqual Right(
      List(For("for", line(1)), eofToken())
    )
    Scanner.scanTokens("if") shouldEqual Right(
      List(If("if", line(1)), eofToken())
    )
    Scanner.scanTokens("nil") shouldEqual Right(
      List(Nil("nil", line(1)), eofToken())
    )
    Scanner.scanTokens("or") shouldEqual Right(
      List(Or("or", line(1)), eofToken())
    )
    Scanner.scanTokens("print") shouldEqual Right(
      List(Print("print", line(1)), eofToken())
    )
    Scanner.scanTokens("return") shouldEqual Right(
      List(Return("return", line(1)), eofToken())
    )
    Scanner.scanTokens("super") shouldEqual Right(
      List(Super("super", line(1)), eofToken())
    )
    Scanner.scanTokens("this") shouldEqual Right(
      List(This("this", line(1)), eofToken())
    )
    Scanner.scanTokens("true") shouldEqual Right(
      List(True("true", line(1)), eofToken())
    )
    Scanner.scanTokens("var") shouldEqual Right(
      List(Var("var", line(1)), eofToken())
    )
    Scanner.scanTokens("while") shouldEqual Right(
      List(While("while", line(1)), eofToken())
    )
  }

  it should "scan one-line strings" in {
    Scanner.scanTokens("\"some text\"") shouldEqual Right(
      List(StringToken("\"some text\"", "some text", line(1)), eofToken())
    )
  }

  it should "scan multi-line strings" in {
    val expectedLiteralValue =
      """really
        |long
        |text""".stripMargin

    val tokens =
      Scanner.scanTokens(""""really
          |long
          |text"""".stripMargin) shouldEqual Right(
        List(
          StringToken(
            """"really
            |long
            |text"""".stripMargin,
            expectedLiteralValue,
            line(1)
          ),
          eofToken(3)
        )
      )
  }

  it should "return error on unterminated strings" in {
    Scanner.scanTokens("\"unfinished stri") shouldEqual Left(
      ScannerError(line(1), "Unterminated string.")
    )

    Scanner.scanTokens(""""long
        |unfinished stri""".stripMargin) shouldEqual Left(
      ScannerError(line(2), "Unterminated string.")
    )
  }

  it should "scan identifiers" in {
    Scanner.scanTokens("a") shouldEqual Right(
      List(IdentifierToken("a", "a", line(1)), eofToken())
    )
    Scanner.scanTokens("a_") shouldEqual Right(
      List(IdentifierToken("a_", "a_", line(1)), eofToken())
    )
    Scanner.scanTokens("ab") shouldEqual Right(
      List(IdentifierToken("ab", "ab", line(1)), eofToken())
    )
    Scanner.scanTokens("a_b") shouldEqual Right(
      List(IdentifierToken("a_b", "a_b", line(1)), eofToken())
    )
    Scanner.scanTokens("ab2_") shouldEqual Right(
      List(IdentifierToken("ab2_", "ab2_", line(1)), eofToken())
    )
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

    Scanner.scanTokens(source) shouldEqual Right(
      List(
        LeftParen("(", line(2)),
        LeftParen("(", line(2)),
        RightParen(")", line(2)),
        RightParen(")", line(2)),
        LeftBrace("{", line(2)),
        RightBrace("}", line(2)),
        Bang("!", line(3)),
        Star("*", line(3)),
        Plus("+", line(3)),
        Minus("-", line(3)),
        Slash("/", line(3)),
        Equal("=", line(3)),
        Less("<", line(3)),
        Greater(">", line(3)),
        LessEqual("<=", line(3)),
        EqualEqual("==", line(3)),
        IdentifierToken("a", "a", line(4)),
        Dot(".", line(4)),
        IdentifierToken("b", "b", line(4)),
        Plus("+", line(4)),
        StringToken("\"str\"", "str", line(4)),
        Star("*", line(4)),
        NumberToken("2.3", 2.3, line(4)),
        eofToken(4)
      )
    )
  }

  it should "return error on unexpected symbols" in {
    Scanner.scanTokens("A + ~b") shouldEqual Left(
      ScannerError(line(1), "Unexpected character '~'.")
    )
  }
}
