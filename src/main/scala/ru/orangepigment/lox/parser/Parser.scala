package ru.orangepigment.lox.parser

import ru.orangepigment.lox.ast.{Binary, BooleanLiteral, Expr, Grouping, Literal, NilLiteral, NumberLiteral, StringLiteral, Unary}
import ru.orangepigment.lox.errors.ParserError
import ru.orangepigment.lox.scanning.*
import ru.orangepigment.lox.scanning.TokenType.*

import scala.util.control.TailCalls.*

object Parser {

  // ToDo: keep a list of errors
  private type RuleResult = Either[ParserError, (Int, Expr)]

  def parse(tokens: Array[Token]): Either[ParserError, Expr] =
    expression(tokens, 0).result.map(_._2)

  private def expression(tokens: Array[Token], current: Int): TailRec[RuleResult] = equality(tokens, current)

  private def equality(tokens: Array[Token], current: Int): TailRec[RuleResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[RuleResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case BangEqual(_, _) | EqualEqual(_, _) =>
            nestedTailCall(comparison(tokens, current + 1)) { case (next, right) =>
              val eqExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, eqExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedTailCall(comparison(tokens, current)) {
      loop(tokens)
    }
  }

  private def comparison(tokens: Array[Token], current: Int): TailRec[RuleResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[RuleResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Greater(_, _) | GreaterEqual(_, _) | Less(_, _) | LessEqual(_, _) =>
            nestedTailCall(term(tokens, current + 1)) { case (next, right) =>
              val compExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, compExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedTailCall(term(tokens, current)) {
      loop(tokens)
    }
  }

  private def term(tokens: Array[Token], current: Int): TailRec[RuleResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[RuleResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Minus(_, _) | Plus(_, _) =>
            nestedTailCall(factor(tokens, current + 1)) { case (next, right) =>
              val termExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, termExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedTailCall(factor(tokens, current)) {
      loop(tokens)
    }
  }

  private def factor(tokens: Array[Token], current: Int): TailRec[RuleResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[RuleResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Slash(_, _) | Star(_, _) =>
            nestedTailCall(unary(tokens, current + 1)) { case (next, right) =>
              val factorExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, factorExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedTailCall(unary(tokens, current)) {
      loop(tokens)
    }
  }

  private def unary(tokens: Array[Token], current: Int): TailRec[RuleResult] = {
    if (current < tokens.length)
      val operator = tokens(current)
      operator match {
        case Bang(_, _) | Minus(_, _) =>
          tailcall(unary(tokens, current + 1)).map {
            case l@Left(error) => l
            case Right((next, expr)) => Right(next -> Unary(operator.asInstanceOf[UnaryOp], expr))
          }
        case _ => tailcall(primary(tokens, current))
      }
    else
      tailcall(primary(tokens, current))
  }

  private def primary(tokens: Array[Token], current: Int): TailRec[RuleResult] = {
    if (current < tokens.length) {
      val token = tokens(current)
      token match {
        case False(_, _) => done(Right((current + 1) -> BooleanLiteral(false)))
        case True(_, _) => done(Right((current + 1) -> BooleanLiteral(true)))
        case Nil(_, _) => done(Right((current + 1) -> NilLiteral))
        case Number(_, raw, _) => done(Right((current + 1) -> NumberLiteral(raw)))
        case StringToken(_, raw, _) => done(Right((current + 1) -> StringLiteral(raw)))
        case LeftParen(_, _) =>
          tailcall(expression(tokens, current + 1)).flatMap {
            case l@Left(error) => done(l)
            case Right((next, expr)) =>
              if (next < tokens.length) {
                tokens(next) match {
                  case RightParen(_, _) => done(Right((next + 1) -> Grouping(expr)))
                  case wrongToken => done(Left(ParserError(wrongToken, s"Expected ')' after expression, got $wrongToken instead.")))
                }
              } else {
                done(unexpectedEndOfInput(tokens, next))
              }
          }
        case wrongToken => done(Left(ParserError(wrongToken, s"Expected a literal or '(', got $wrongToken instead.")))
      }
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def unexpectedEndOfInput(tokens: Array[Token], current: Int): Either[ParserError, (Int, Expr)] =
    Left(ParserError(tokens(current - 1), "Expected expression, but got end of file."))

  // private def synchronize // ToDo: implement in later chapters

  private def nestedTailCall(call: => TailRec[RuleResult])(onSuccess: (Int, Expr) => TailRec[RuleResult]) =
    tailcall(call).flatMap {
      case l@Left(error) => done(l)
      case Right((next, right)) => tailcall(onSuccess(next, right))
    }

}
