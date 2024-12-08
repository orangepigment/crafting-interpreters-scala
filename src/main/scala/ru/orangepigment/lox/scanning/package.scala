package ru.orangepigment.lox

import ru.orangepigment.lox.scanning.Token
import ru.orangepigment.lox.scanning.TokenType.*

import scala.annotation.targetName

package object scanning {
  opaque type LineNum = Int

  object LineNum {
    def apply(value: Int): LineNum = value

    extension (lineNum: LineNum) {
      def int: Int = lineNum

      @targetName("add")
      def +(i: Int): LineNum = lineNum + i
    }
  }

  opaque type Position = Int

  object Position {
    def apply(value: Int): Position = value

    extension (position: Position) {
      def int: Int = position

      @targetName("add")
      def +(i: Int): Position = position + i

      @targetName("subtract")
      def -(i: Int): Position = position - i

      @targetName("greaterOrEqual")
      def >=(i: Int): Boolean = position >= i

      @targetName("less")
      def <(i: Int): Boolean = position < i
    }
  }

}
