package ru.orangepigment.lox.interpreter

import ru.orangepigment.lox.ast.{Binary, BooleanLiteral, Expr, Grouping, IdentifierLiteral, NilLiteral, NumberLiteral, StringLiteral, Unary}
import ru.orangepigment.lox.errors.RuntimeError
import ru.orangepigment.lox.scanning
import ru.orangepigment.lox.scanning.{And, Bang, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, Less, LessEqual, Minus, Or, Plus, Slash, Star}

import scala.util.control.TailCalls.*

object Interpreter {

  // ToDo: handle Runtime errors
  def interpret(expr: Expr): Either[RuntimeError, Unit] = {
    def walk(expr: Expr): TailRec[Either[RuntimeError, Option[AnyVal]]] = {
      expr match
        case Unary(operator, expr) => tailcall(walk(expr)).map { e =>
          e.flatMap { value =>
            operator match
              case Minus(_, _) =>
                value match
                  case Some(d: Double) => Right(Option(-d))
                  case _ => Left(RuntimeError(operator, "Operand must be a number."))
              case Bang(_, _) => Right(Option(!isTruthy(value)))
          }
        }
        case Binary(left, operator, right) =>
          // ToDo: add nested for comprehension for unpacking le and re
          //  And move it to a helper
          for {
            le <- tailcall(walk(left))
            lr <- tailcall(walk(right))
          } yield {
            operator match
              case Minus(_, _) =>
                l.zip(r).map(_.asInstanceOf[Double] - _.asInstanceOf[Double])
              case Plus(_, _) =>
                l.zip(r) match
                  case Some(ld: Double, rd: Double) => Option(ld + rd)
                  case Some(ls: String, rs: String) => Option(ls + rs)
                  case None => Right(Option.empty[AnyVal])
                  case _ => Left(RuntimeError(operator, "Operands must be two numbers or two strings."))
              case Slash(_, _) => l.zip(r).map(_.asInstanceOf[Double] / _.asInstanceOf[Double])
              case Star(_, _) => l.zip(r).map(_.asInstanceOf[Double] * _.asInstanceOf[Double])
              // ToDo: add cases for other binary ops
              case BangEqual(_, _) => ???
              case Equal(_, _) => ???
              case EqualEqual(_, _) => ???
              case Greater(_, _) => ???
              case GreaterEqual(_, _) => ???
              case Less(_, _) => ???
              case LessEqual(_, _) => ???
              case And(_, _) => ???
              case Or(_, _) => ???
          }
        case Grouping(expression) => tailcall(walk(expression))
        case StringLiteral(raw) => done(Right(Option(raw)))
        case NumberLiteral(raw) => done(Right(Option(raw)))
        case BooleanLiteral(raw) => done(Right(Option(raw)))
        case IdentifierLiteral(raw) => done(Right(Option(raw)))
        case NilLiteral => done(Right(Option.empty[AnyVal]))
    }

    for {
      result <- walk(expr).result
      // ToDo: print result, stringify as in the book?
    } yield println(result)
  }

  private def isTruthy(obj: Option[AnyVal]): Boolean =
    obj match
      case Some(bool: Boolean) => bool
      case Some(_) => true
      case None => false

}
