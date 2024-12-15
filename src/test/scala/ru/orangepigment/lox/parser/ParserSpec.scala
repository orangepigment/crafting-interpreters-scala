package ru.orangepigment.lox.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.ast.*
import ru.orangepigment.lox.errors.{ParserError, ParserErrors}
import ru.orangepigment.lox.scanning.*

class ParserSpec extends AnyFlatSpec with Matchers {
  "Parser.parse" should "correctly parse tokens into an expression" in {
    val input: Array[Token] = Array(
      Minus("-", LineNum(1)),
      Number("123", 123, LineNum(1)),
      Star("*", LineNum(1)),
      LeftParen("(", LineNum(1)),
      Number("45.67", 45.67, LineNum(1)),
      RightParen(")", LineNum(1)),
      Semicolon(";", LineNum(1)),
      EOF("", LineNum(1))
    )
    val expected: List[Stmt] = List(
      ExpressionStmt(
        Binary(
          Unary(Minus("-", LineNum(1)), NumberLiteral(123)),
          Star("*", LineNum(1)),
          Grouping(NumberLiteral(45.67))
        )
      )
    )

    Parser.parse(input) shouldEqual Right(expected)
  }

  it should "return error when the input is invalid" in {
    val input: Array[Token] = Array(
      LeftParen("(", LineNum(1)),
      Number("1", 1, LineNum(1)),
      EOF("", LineNum(1))
    )
    val expected: ParserErrors = ParserErrors(
      List(
        ParserError(
          EOF("", LineNum(1)),
          "Expected ')' after expression, got L1: EOF  instead."
        )
      )
    )
    Parser.parse(input) shouldEqual Left(expected)

    val input2: Array[Token] = Array(
      LeftParen("(", LineNum(1)),
      Number("1", 1, LineNum(1)),
      Plus("+", LineNum(1)),
      EOF("", LineNum(1))
    )
    val expected2: ParserErrors = ParserErrors(
      List(
        ParserError(
          EOF("", LineNum(1)),
          "Expected a literal or '(', got L1: EOF  instead."
        )
      )
    )
    Parser.parse(input2) shouldEqual Left(expected2)
  }

  it should "return all errors when the input is invalid" in {
    val input: Array[Token] = Array(
      LeftParen("(", LineNum(1)),
      Number("1", 1, LineNum(1)),
      Semicolon(";", LineNum(1)),
      Number("1", 1, LineNum(2)),
      Semicolon(";", LineNum(2)),
      Minus("-", LineNum(3)),
      StringToken("\"abc\"", "abc", LineNum(3)),
      EOF("", LineNum(3))
    )
    val expected: ParserErrors = ParserErrors(
      List(
        ParserError(
          Semicolon(";", LineNum(1)),
          "Expected ')' after expression, got L1: Semicolon ; instead."
        ),
        ParserError(
          EOF("", LineNum(3)),
          "Expected ';' after expression, got L3: EOF  instead."
        )
      )
    )
    Parser.parse(input) shouldEqual Left(expected)
  }
}
