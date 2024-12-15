package ru.orangepigment.lox

import org.scalacheck.Gen
import ru.orangepigment.lox.ast.{
  BooleanLiteral,
  Literal,
  NilLiteral,
  NumberLiteral,
  StringLiteral
}

package object gen {

  val genNumberLiteral: Gen[NumberLiteral] = Gen.double.map(NumberLiteral.apply)
  val genStringLiteral: Gen[StringLiteral] =
    Gen
      .stringOf(
        Gen.asciiChar.filter(_ != '"')
      ) // Lox doesn't support quotes inside a string
      .map(StringLiteral.apply)
  val genBooleanLiteral: Gen[BooleanLiteral] =
    Gen.oneOf(true, false).map(BooleanLiteral.apply)
  val genNil: Gen[NilLiteral.type] = Gen.const(NilLiteral)
  val genAnyLiteral: Gen[Literal] =
    Gen.oneOf(genNumberLiteral, genStringLiteral, genBooleanLiteral, genNil)
  val genNonNumber: Gen[Literal] =
    Gen.oneOf(genStringLiteral, genBooleanLiteral, genNil)
  val genNonString: Gen[Literal] =
    Gen.oneOf(genNumberLiteral, genBooleanLiteral, genNil)
  val genNonNumberNorString: Gen[Literal] = Gen.oneOf(genBooleanLiteral, genNil)
  val genNonNil: Gen[Literal] =
    Gen.oneOf(genNumberLiteral, genStringLiteral, genBooleanLiteral)

}
