package ru.orangepigment.lox.parser

import ru.orangepigment.lox.ast.*
import ru.orangepigment.lox.errors.ParserError
import ru.orangepigment.lox.scanning.*

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

object Parser {

  // ToDo: keep a list of errors
  private type ExprResult = Either[ParserError, (Int, Expr)]
  private type StmtResult = Either[ParserError, (Int, Stmt)]
  private type StmtListResult = Either[ParserError, List[Stmt]]

  def parse(tokens: Array[Token]): StmtListResult = {
    program(tokens, 0, List.empty).result.map(_.reverse)
  }

  private def program(tokens: Array[Token], current: Int, statements: List[Stmt]): TailRec[StmtListResult] =
    if (current < tokens.length) {
      tokens(current) match
        case EOF(_, _) => done(Right(statements))
        case _ =>
          declaration(tokens, current).flatMap {
            _.fold(
              e => {
                // ToDo: Ideally we want to keep either a list of error or a list of statements.
                //  Do not rely on Either. Make ADT. trait ParserResult; case StatementsResult; case ParserErrorsResult
                // ToDo: synchronize in case of error. Basically recover.
                //  Provide correct current position
                //val afterError = synchronize(tokens, -1)
                //program(tokens, afterError, statements)
                done(Left(e))
              },
              (i, stmt) => program(tokens, i, stmt +: statements)
            )
          }
    } else {
      done(Right(statements))
    }

  private def declaration(tokens: Array[Token], current: Int): TailRec[StmtResult] =
    if (current < tokens.length) {
      tokens(current) match
        case Var(_, _) =>
          tokens(current + 1) match {
            case Identifier(_, raw, _) =>
              val varName = IdentifierLiteral(raw)
              tokens(current + 1) match {
                case Equal(_, _) =>
                  expression(tokens, current + 2).map { exprResult =>
                    exprResult.map { case (i, expr) => i -> VarDeclStmt(varName, Option(expr)) }
                  }
                case Semicolon(_, _) => done(Right((current + 2) -> VarDeclStmt(varName, None)))
                case wrongToken => done(Left(ParserError(wrongToken, s"Expected ';' after variable declaration, got $wrongToken instead.")))
              }
            case wrongToken => done(Left(ParserError(wrongToken, s"Expected variable name, got $wrongToken instead.")))
          }
        case _ => statement(tokens, current)
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }

  private def statement(tokens: Array[Token], current: Int): TailRec[StmtResult] = {
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

  private def printStatement(tokens: Array[Token], current: Int): TailRec[StmtResult] = {
    if (current < tokens.length) {
      val exprResult = expression(tokens, current).result
      done(exprResult.flatMap { case (next, expr) =>
        tokens(next) match
          case Semicolon(_, _) => Right((next + 1) -> PrintStmt(expr))
          case wrongToken => Left(ParserError(wrongToken, s"Expected ';' after expression, got $wrongToken instead."))
      })
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def expressionStatement(tokens: Array[Token], current: Int): TailRec[StmtResult] = {
    if (current < tokens.length) {
      val exprResult = expression(tokens, current).result
      done(exprResult.flatMap { case (next, expr) =>
        tokens(next) match
          case Semicolon(_, _) => Right((next + 1) -> ExpressionStmt(expr))
          case wrongToken => Left(ParserError(wrongToken, s"Expected ';' after expression, got $wrongToken instead."))
      })
    } else {
      done(unexpectedEndOfInput(tokens, current))
    }
  }

  private def expression(tokens: Array[Token], current: Int): TailRec[ExprResult] = equality(tokens, current)

  private def equality(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case BangEqual(_, _) | EqualEqual(_, _) =>
            nestedExprTailCall(comparison(tokens, current + 1)) { case (next, right) =>
              val eqExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, eqExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedExprTailCall(comparison(tokens, current)) {
      loop(tokens)
    }
  }

  private def comparison(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Greater(_, _) | GreaterEqual(_, _) | Less(_, _) | LessEqual(_, _) =>
            nestedExprTailCall(term(tokens, current + 1)) { case (next, right) =>
              val compExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, compExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedExprTailCall(term(tokens, current)) {
      loop(tokens)
    }
  }

  private def term(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Minus(_, _) | Plus(_, _) =>
            nestedExprTailCall(factor(tokens, current + 1)) { case (next, right) =>
              val termExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, termExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
    }

    nestedExprTailCall(factor(tokens, current)) {
      loop(tokens)
    }
  }

  private def factor(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    def loop(tokens: Array[Token])(current: Int, expr: Expr): TailRec[ExprResult] = {
      if (current < tokens.length)
        val operator = tokens(current)
        operator match {
          case Slash(_, _) | Star(_, _) =>
            nestedExprTailCall(unary(tokens, current + 1)) { case (next, right) =>
              val factorExpr = Binary(expr, operator.asInstanceOf[BinaryOp], right)
              loop(tokens)(next, factorExpr)
            }
          case _ => done(Right(current, expr))
        }
      else
        done(Right(current, expr))
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
            case l@Left(error) => l
            case Right((next, expr)) => Right(next -> Unary(operator.asInstanceOf[UnaryOp], expr))
          }
        case _ => tailcall(primary(tokens, current))
      }
    else
      tailcall(primary(tokens, current))
  }

  private def primary(tokens: Array[Token], current: Int): TailRec[ExprResult] = {
    if (current < tokens.length) {
      val token = tokens(current)
      token match {
        case False(_, _) => done(Right((current + 1) -> BooleanLiteral(false)))
        case True(_, _) => done(Right((current + 1) -> BooleanLiteral(true)))
        case Nil(_, _) => done(Right((current + 1) -> NilLiteral))
        case Number(_, raw, _) => done(Right((current + 1) -> NumberLiteral(raw)))
        case StringToken(_, raw, _) => done(Right((current + 1) -> StringLiteral(raw)))
        case Identifier(_, raw, _) => done(Right((current + 1) -> IdentifierLiteral(raw)))
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

  private def unexpectedEndOfInput[A](tokens: Array[Token], current: Int): Either[ParserError, A] =
    Left(ParserError(tokens(current - 1), "Expected expression, but got end of file."))

  // ToDo: We need to keep current position together with error
  private def synchronize(tokens: Array[Token], current: Int): Int = {
    @tailrec
    def loop(tokens: Array[Token], current: Int): Int = {
      if (current < tokens.length) {
        tokens(current) match
          case _: Class | _: Fun | _: Var | _: For | _: If | _: While | _: Print | _: Return => current
          case _ => loop(tokens, current + 1)
      } else {
        current
      }
    }

    loop(tokens, current + 1)
  }

  private def nestedExprTailCall(call: => TailRec[ExprResult])(onSuccess: (Int, Expr) => TailRec[ExprResult]) =
    tailcall(call).flatMap {
      case l@Left(error) => done(l)
      case Right((next, right)) => tailcall(onSuccess(next, right))
    }

}
