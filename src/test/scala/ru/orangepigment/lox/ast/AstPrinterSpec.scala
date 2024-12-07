package ru.orangepigment.lox.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.scanning.{LineNum, NumberLiteral, Token}
import ru.orangepigment.lox.scanning.TokenType.*

class AstPrinterSpec extends AnyFlatSpec with Matchers {

  "AstPrinter.render" should "render the ast correctly" in {
    val expression =
      Binary(
        Unary(
          Token(MINUS, "-", None, LineNum(1)),
          Literal(NumberLiteral(123))
        ),
        Token(STAR, "*", None, LineNum(1)),
        Grouping(Literal(NumberLiteral(45.67)))
      )

    AstPrinter.render(expression) shouldEqual "(* (- 123.0) (group 45.67))"
  }

  it should "render another ast correctly" in {
    val expression =
      Binary(
        Keyword(FALSE),
        Token(OR, "OR", None, LineNum(1)),
        Keyword(TRUE)
      )

    AstPrinter.render(expression) shouldEqual "(OR FALSE TRUE)"
  }

}
