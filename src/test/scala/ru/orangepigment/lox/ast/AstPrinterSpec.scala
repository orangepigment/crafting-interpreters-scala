package ru.orangepigment.lox.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.scanning.{LineNum, Minus, Or, Star, Token}
import ru.orangepigment.lox.scanning.TokenType.*

class AstPrinterSpec extends AnyFlatSpec with Matchers {

  "AstPrinter.render" should "render the ast correctly" in {
    val expression =
      Binary(
        Unary(
          Minus("-", LineNum(1)),
          NumberLiteral(123)
        ),
        Star("*", LineNum(1)),
        Grouping(NumberLiteral(45.67))
      )

    AstPrinter.render(expression) shouldEqual "(* (- 123.0) (group 45.67))"
  }

  it should "render another ast correctly" in {
    val expression =
      Binary(
        BooleanLiteral(false),
        Or("or", LineNum(1)),
        BooleanLiteral(true)
      )

    AstPrinter.render(expression) shouldEqual "(or false true)"
  }

}
