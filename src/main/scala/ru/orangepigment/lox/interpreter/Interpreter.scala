package ru.orangepigment.lox.interpreter

import ru.orangepigment.lox.ast.{
  Binary,
  BooleanLiteral,
  Expr,
  ExpressionStmt,
  Grouping,
  IdentifierLiteral,
  NilLiteral,
  NumberLiteral,
  PrintStmt,
  Stmt,
  StringLiteral,
  Unary,
  VarDeclStmt
}
import ru.orangepigment.lox.errors.RuntimeError
import ru.orangepigment.lox.scanning.{
  Bang,
  BangEqual,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Minus,
  Plus,
  Slash,
  Star,
  Token
}

import scala.util.control.TailCalls.*

object Interpreter {

  private type InterpreterResult = Either[RuntimeError, Option[Any]]

  // def interpretAndStringify(program: List[Stmt]): Either[RuntimeError, String] =
  //  evaluate(program).map(r => stringify(r))

  private val INIT: Either[RuntimeError, Unit] = Right(())

  private val environment = Environment()

  def interpret(program: List[Stmt]): Either[RuntimeError, Unit] =
    program.foldLeft(INIT) { case (prev, stmt) =>
      prev.flatMap { _ =>
        stmt match
          case ExpressionStmt(expression) => evaluate(expression).map(_ => ())
          case PrintStmt(expression) =>
            evaluate(expression).map(r => println(stringify(r)))
          case VarDeclStmt(name, expression) =>
            expression.fold(
              Right(environment.define(name, Option.empty[Any]))
            ) { expr =>
              evaluate(expr).map(value => environment.define(name, value))
            }
      }
    }

  def evaluate(expr: Expr): InterpreterResult = {
    def walk(expr: Expr): TailRec[InterpreterResult] = {
      expr match
        case Unary(operator, expr) =>
          tailcall(walk(expr)).map { e =>
            e.flatMap { value =>
              operator match
                case Minus(_, _) =>
                  value match
                    case Some(d: Double) => rightOption(-d)
                    case _ =>
                      Left(RuntimeError(operator, "Operand must be a number."))
                case Bang(_, _) => rightOption(!isTruthy(value))
            }
          }
        case Binary(left, operator, right) =>
          for {
            le <- tailcall(walk(left))
            lr <- tailcall(walk(right))
          } yield unpackEithers(le, lr) { (l, r) =>
            operator match
              case Minus(_, _) => checkNumberOperands(operator, l, r)(_ - _)
              case Plus(_, _) =>
                (l, r) match
                  case (Some(ld: Double), Some(rd: Double)) =>
                    rightOption(ld + rd)
                  case (Some(ls: String), Some(rs: String)) =>
                    rightOption(ls + rs)
                  case _ =>
                    Left(
                      RuntimeError(
                        operator,
                        "Operands must be two numbers or two strings."
                      )
                    )
              case Slash(_, _)     => checkNumberOperands(operator, l, r)(_ / _)
              case Star(_, _)      => checkNumberOperands(operator, l, r)(_ * _)
              case BangEqual(_, _) => rightOption(!isEqual(l, r))
              case EqualEqual(_, _) => rightOption(isEqual(l, r))
              case Greater(_, _) => checkNumberOperands(operator, l, r)(_ > _)
              case GreaterEqual(_, _) =>
                checkNumberOperands(operator, l, r)(_ >= _)
              case Less(_, _) => checkNumberOperands(operator, l, r)(_ < _)
              case LessEqual(_, _) =>
                checkNumberOperands(operator, l, r)(_ <= _)
            // case And(_, _) => rightOption(isTruthy(l) && isTruthy(r))
            // case Or(_, _) => rightOption(isTruthy(l) || isTruthy(r))
          }
        case Grouping(expression)     => tailcall(walk(expression))
        case StringLiteral(raw)       => done(Right(Option(raw)))
        case NumberLiteral(raw)       => done(Right(Option(raw)))
        case BooleanLiteral(raw)      => done(Right(Option(raw)))
        case IdentifierLiteral(label) => done(environment.get(label))
        case NilLiteral               => done(Right(Option.empty[Any]))
    }

    for {
      result <- walk(expr).result
    } yield result
  }

  private def stringify(obj: Option[Any]): String =
    obj match
      case Some(d: Double) =>
        val str = d.toString
        if (str.endsWith(".0")) str.dropRight(2) else str
      case Some(value) => value.toString
      case None        => "nil"

  private def isTruthy(obj: Option[Any]): Boolean =
    obj match
      case Some(bool: Boolean) => bool
      case Some(_)             => true
      case None                => false

  private def isEqual(a: Option[Any], b: Option[Any]): Boolean =
    (a, b) match {
      case (None, None)             => true
      case (Some(aVal), Some(bVal)) => aVal.equals(bVal)
      case _                        => false
    }

  private def checkNumberOperands[A](
    operator: Token,
    l: Option[Any],
    r: Option[Any]
  )(op: (Double, Double) => A): InterpreterResult =
    (l, r) match
      case (Some(ld: Double), Some(rd: Double)) => rightOption(op(ld, rd))
      case _ => Left(RuntimeError(operator, "Operands must be two numbers."))

  // Helpers for building results
  private def unpackEithers(le: InterpreterResult, re: InterpreterResult)(
    action: (Option[Any], Option[Any]) => InterpreterResult
  ): InterpreterResult =
    le.flatMap(l => re.flatMap(r => action(l, r)))

  private def rightOption(value: Any): InterpreterResult =
    Right(Option(value))

}
