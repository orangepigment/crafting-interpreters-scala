package ru.orangepigment.lox.parser

import ru.orangepigment.lox.ast.*
import ru.orangepigment.lox.errors.{ParserError, ParserErrors}
import ru.orangepigment.lox.scanning.*

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

object Parser {

  // ToDo: use position instead of int?
  private type ExprResult = Either[(Int, ParserError), (Int, Expr)]
  private type StmtResult = Either[(Int, ParserError), (Int, Stmt)]

  def parse(tokens: Array[Token]): Either[ParserErrors, List[Stmt]] = {
    program(tokens, 0, List.empty, List.empty).result
      .map(_.reverse)
      .left
      .map(le => ParserErrors.apply(le.reverse))
  }

  private def program(
    tokens: Array[Token],
    current: Int,
    statements: List[Stmt],
    errors: List[ParserError]
  ): TailRec[Either[List[ParserError], List[Stmt]]] =
    if (current < tokens.length) {
      tokens(current) match
        case EOF(_, _) =>
          if (errors.isEmpty) done(Right(statements)) else done(Left(errors))
        case _ =>
          declaration(tokens, current).flatMap {
            _.fold(
              (i, e) => {
                val afterError = synchronize(tokens, i)
                program(tokens, afterError, statements, e +: errors)
              },
              (i, stmt) => program(tokens, i, stmt +: statements, errors)
            )
          }
    } else {
      if (errors.isEmpty) done(Right(statements)) else done(Left(errors))
    }

  private def declaration(
    tokens: Array[Token],
    current: Int
  ): TailRec[StmtResult] =
    if (current < tokens.length) {
      tokens(current) match
        case Var(_, _) =>
          tokens(current + 1) match {
            case varName @ IdentifierToken(_, raw, _) =>
              tokens(current + 2) match {
                case Equal(_, _) =>
                  expression(tokens, current + 3).map { exprResult =>
                    exprResult.flatMap { case (i, expr) =>
                      tokens(i) match {
                        case Semicolon(_, _) =>
                          Right(i + 1 -> VarDeclStmt(varName, Option(expr)))
                        case wrongToken =>
                          Left(
                            i + 1 -> ParserError(
                              wrongToken,
                              s"Expected ';' after variable declaration, got $wrongToken instead."
                            )
                          )
                      }
                    }
                  }
                case Semicolon(_, _) =>
                  done(Right(current + 3 -> VarDeclStmt(varName, None)))
                case wrongToken =>
                  done(
                    Left(
                      current + 2 -> ParserError(
                        wrongToken,
                        s"Expected ';' after variable declaration, got $wrongToken instead."
                      )
                    )
                  )
              }
            case wrongToken =>
              done(
                Left(
                  current + 1 -> ParserError(
                    wrongToken,
                    s"Expected variable name, got $wrongToken instead."
                  )
                )
              )
          }
        case _ => statement(tokens, current)
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }

  private def statement(
    tokens: Array[Token],
    current: Int
  ): TailRec[StmtResult] = {
    if (current < tokens.length) {
      tokens(current) match
        case Print(_, _) =>
          printStatement(tokens, current + 1)
        case _ =>
          expressionStatement(tokens, current)
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def printStatement(
    tokens: Array[Token],
    current: Int
  ): TailRec[StmtResult] = {
    if (current < tokens.length) {
      val exprResult = expression(tokens, current).result
      done(exprResult.flatMap { case (next, expr) =>
        tokens(next) match
          case Semicolon(_, _) => Right((next + 1) -> PrintStmt(expr))
          case wrongToken =>
            Left(
              next -> ParserError(
                wrongToken,
                s"Expected ';' after expression, got $wrongToken instead."
              )
            )
      })
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def expressionStatement(
    tokens: Array[Token],
    current: Int
  ): TailRec[StmtResult] = {
    if (current < tokens.length) {
      val exprResult = expression(tokens, current).result
      done(exprResult.flatMap { case (next, expr) =>
        tokens(next) match
          case Semicolon(_, _) => Right((next + 1) -> ExpressionStmt(expr))
          case wrongToken =>
            Left(
              next -> ParserError(
                wrongToken,
                s"Expected ';' after expression, got $wrongToken instead."
              )
            )
      })
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def expression(
    tokens: Array[Token],
    current: Int
  ): TailRec[ExprResult] = equality(tokens, current)

  private def equality(
    tokens: Array[Token],
    current: Int
  ): TailRec[ExprResult] = {
    def loop(
      tokens: Array[Token]
    )(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case BangEqual(_, _) | EqualEqual(_, _) =>
            nestedExprTailCall(comparison(tokens, current + 1)) {
              case (next, right) =>
                val eqExpr =
                  Binary(expr, operator.asInstanceOf[BinaryOp], right)
                loop(tokens)(next, eqExpr)
            }
          case _ => done(Right(current, expr))
        }
      else done(Right(current, expr))
    }

    nestedExprTailCall(comparison(tokens, current)) {
      loop(tokens)
    }
  }

  private def comparison(
    tokens: Array[Token],
    current: Int
  ): TailRec[ExprResult] = {
    def loop(
      tokens: Array[Token]
    )(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Greater(_, _) | GreaterEqual(_, _) | Less(_, _) |
              LessEqual(_, _) =>
            nestedExprTailCall(term(tokens, current + 1)) {
              case (next, right) =>
                val compExpr =
                  Binary(expr, operator.asInstanceOf[BinaryOp], right)
                loop(tokens)(next, compExpr)
            }
          case _ => done(Right(current, expr))
        }
      else done(Right(current, expr))
    }

    nestedExprTailCall(term(tokens, current)) {
      loop(tokens)
    }
  }

  private def term(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    def loop(
      tokens: Array[Token]
    )(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Minus(_, _) | Plus(_, _) =>
            nestedExprTailCall(factor(tokens, current + 1)) {
              case (next, right) =>
                val termExpr =
                  Binary(expr, operator.asInstanceOf[BinaryOp], right)
                loop(tokens)(next, termExpr)
            }
          case _ => done(Right(current, expr))
        }
      else done(Right(current, expr))
    }

    nestedExprTailCall(factor(tokens, current)) {
      loop(tokens)
    }
  }

  private def factor(
    tokens: Array[Token],
    current: Int
  ): TailRec[ExprResult] = {
    def loop(
      tokens: Array[Token]
    )(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Slash(_, _) | Star(_, _) =>
            nestedExprTailCall(unary(tokens, current + 1)) {
              case (next, right) =>
                val factorExpr =
                  Binary(expr, operator.asInstanceOf[BinaryOp], right)
                loop(tokens)(next, factorExpr)
            }
          case _ => done(Right(current, expr))
        }
      else done(Right(current, expr))
    }

    nestedExprTailCall(unary(tokens, current)) {
      loop(tokens)
    }
  }

  private def unary(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    if (current < tokens.length)
      val operator = tokens(current)
      operator match {
        case Bang(_, _) | Minus(_, _) =>
          tailcall(unary(tokens, current + 1)).map {
            case l @ Left(error) => l
            case Right((next, expr)) =>
              Right(next -> Unary(operator.asInstanceOf[UnaryOp], expr))
          }
        case _ => tailcall(primary(tokens, current))
      }
    else tailcall(primary(tokens, current))
  }

  private def primary(
    tokens: Array[Token],
    current: Int
  ): TailRec[ExprResult] = {
    if (current < tokens.length) {
      val token = tokens(current)
      token match {
        case False(_, _) => done(Right((current + 1) -> BooleanLiteral(false)))
        case True(_, _)  => done(Right((current + 1) -> BooleanLiteral(true)))
        case Nil(_, _)   => done(Right((current + 1) -> NilLiteral))
        case NumberToken(_, raw, _) =>
          done(Right((current + 1) -> NumberLiteral(raw)))
        case StringToken(_, raw, _) =>
          done(Right((current + 1) -> StringLiteral(raw)))
        case label @ IdentifierToken(_, raw, _) =>
          done(Right((current + 1) -> IdentifierLiteral(label)))
        case LeftParen(_, _) =>
          tailcall(expression(tokens, current + 1)).flatMap {
            case l @ Left(error) => done(l)
            case Right((next, expr)) =>
              if (next < tokens.length) {
                tokens(next) match {
                  case RightParen(_, _) =>
                    done(Right((next + 1) -> Grouping(expr)))
                  case wrongToken =>
                    done(
                      Left(
                        next -> ParserError(
                          wrongToken,
                          s"Expected ')' after expression, got $wrongToken instead."
                        )
                      )
                    )
                }
              } else {
                done(unexpectedEndOfInput(tokens, next))
              }
          }
        case wrongToken =>
          done(
            Left(
              current -> ParserError(
                wrongToken,
                s"Expected a literal or '(', got $wrongToken instead."
              )
            )
          )
      }
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def unexpectedEndOfInput[A](
    tokens: Array[Token],
    current: Int
  ): Either[(Int, ParserError), A] =
    Left(
      current -> ParserError(
        tokens(current - 1),
        "Expected expression, but got end of file."
      )
    )

  private def synchronize(tokens: Array[Token], current: Int): Int = {
    @tailrec
    def loop(tokens: Array[Token], current: Int): Int = {
      if (current < tokens.length) {
        tokens(current) match
          case _: Semicolon => current + 1
          case _: Class | _: Fun | _: Var | _: For | _: If | _: While |
              _: Print | _: Return =>
            current
          case _ => loop(tokens, current + 1)
      } else {
        current
      }
    }

    loop(tokens, current)
  }

  private def nestedExprTailCall(
    call: => TailRec[ExprResult]
  )(onSuccess: (Int, Expr) => TailRec[ExprResult]) =
    tailcall(call).flatMap {
      case l @ Left(error)      => done(l)
      case Right((next, right)) => tailcall(onSuccess(next, right))
    }

}
