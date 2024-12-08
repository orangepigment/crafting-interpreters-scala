package ru.orangepigment.lox.ast

import scala.util.control.TailCalls.*

object AstPrinter {
  def print(expr: Expr): Unit = println(render(expr))

  def render(expr: Expr): String = {
    def walk(expr: Expr): TailRec[String] = {
      expr match
        case Binary(left, operator, right) =>
          for {
            l <- tailcall(walk(left))
            r <- tailcall(walk(right))
          } yield s"(${operator.lexeme} $l $r)"
        case Grouping(expression) => tailcall(walk(expression).map(r => s"(group $r)"))
        case StringLiteral(raw) => done(raw)
        case NumberLiteral(raw) => done(raw.toString)
        case BooleanLiteral(raw) => done(raw.toString)
        case IdentifierLiteral(raw) => done(raw)
        case NilLiteral => done("nil")
        case Unary(operator, right) => tailcall(walk(right)).map(r => s"(${operator.lexeme} $r)")
    }

    walk(expr).result
  }
}
