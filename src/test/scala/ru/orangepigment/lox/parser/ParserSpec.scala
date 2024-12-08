package ru.orangepigment.lox.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.ast.*
import ru.orangepigment.lox.scanning.*

class ParserSpec extends AnyFlatSpec with Matchers {
  "Parser.parse" should "correctly parse tokens into an expression" in {
    val input: Array[Token] = Array(
      Number("1", 1, LineNum(1)),
      Plus("+", LineNum(1)),
      Number("2", 2, LineNum(1)),
      EOF("", LineNum(1))
    )
    val expected: Expr = Binary(
      NumberLiteral(1),
      Plus("+", LineNum(1)),
      NumberLiteral(2)
    )
    Parser.parse(input) shouldEqual Right(expected)
  }

  it should "return error when the input is invalid" in {
    val input: Array[Token] = Array(
      LeftParen("(", LineNum(1)),
      Number("1", 1, LineNum(1)),
      EOF("", LineNum(1))
    )
    val expected: ParserError = ParserError(EOF("", LineNum(1)), "Expected ')' after expression, got L1: EOF  instead.")
    Parser.parse(input) shouldEqual Left(expected)

    val input2: Array[Token] = Array(
      LeftParen("(", LineNum(1)),
      Number("1", 1, LineNum(1)),
      Plus("+", LineNum(1)),
      EOF("", LineNum(1))
    )
    val expected2: ParserError = ParserError(EOF("", LineNum(1)), "Expected a literal or '(', got L1: EOF  instead.")
    Parser.parse(input2) shouldEqual Left(expected2)
  }
}
