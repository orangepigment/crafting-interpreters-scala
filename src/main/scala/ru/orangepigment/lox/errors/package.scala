package ru.orangepigment.lox

package object errors {

  extension [A](either: Either[LoxError, A]) {
    def tapError(action: LoxError => Unit): Either[LoxError, A] = {
      either.left.foreach(action)
      either
    }
  }

}
