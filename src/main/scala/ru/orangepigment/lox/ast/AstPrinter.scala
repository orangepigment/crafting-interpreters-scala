package ru.orangepigment.lox.ast

import ru.orangepigment.lox.scanning.{IdentifierLiteral, NumberLiteral, StringLiteral}

import scala.util.control.TailCalls.{TailRec, done}

object AstPrinter {
  def print(expr: Expr): String = render(expr)

  def render(expr: Expr): String = {
    def walk(expr: Expr): TailRec[String] = {
      expr match
        case Binary(left, operator, right) =>
          for {
            l <- walk(left)
            r <- walk(right)
          } yield s"(${operator.lexeme} $l $r)"
        case Grouping(expression) => walk(expression).map(r => s"(group $r)")
        case Literal(literal) =>
          done {
            literal match
              case StringLiteral(raw) => raw
              case NumberLiteral(raw) => raw.toString
              case IdentifierLiteral(raw) => raw
          }
        case Keyword(word) => done(word.toString)
        case Unary(operator, right) => walk(right).map(r => s"(${operator.lexeme} $r)")
    }

    walk(expr).result
  }
}
