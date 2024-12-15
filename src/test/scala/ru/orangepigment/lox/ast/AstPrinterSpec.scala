package ru.orangepigment.lox.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.scanning.{Bang, LineNum, Minus, Star}

class AstPrinterSpec extends AnyFlatSpec with Matchers {

  "AstPrinter.render" should "render the ast correctly" in {
    val expression =
      Binary(
        Unary(Minus("-", LineNum(1)), NumberLiteral(123)),
        Star("*", LineNum(1)),
        Grouping(NumberLiteral(45.67))
      )

    AstPrinter.render(expression) shouldEqual "(* (- 123.0) (group 45.67))"
  }

  it should "render another ast correctly" in {
    val expression =
      Unary(Bang("!", LineNum(1)), BooleanLiteral(false))

    AstPrinter.render(expression) shouldEqual "(! false)"
  }

}
