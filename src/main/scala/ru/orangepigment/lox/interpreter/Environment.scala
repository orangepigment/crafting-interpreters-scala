package ru.orangepigment.lox.interpreter

import ru.orangepigment.lox.errors.RuntimeError
import ru.orangepigment.lox.scanning.IdentifierToken

import scala.collection.mutable

class Environment {
  private val values: mutable.Map[String, Option[Any]] = mutable.Map.empty

  def define(label: IdentifierToken, value: Option[Any]): Unit = {
    values.put(label.lexeme, value)
  }

  def get(label: IdentifierToken): Either[RuntimeError, Option[Any]] = {
    // We can't use .get because `values` can contain None
    if (values.contains(label.lexeme)) {
      Right(values(label.lexeme))
    } else {
      Left(RuntimeError(label, s"Undefined variable '${label.lexeme}'."))
    }
  }
}
