package ru.orangepigment.lox.interpreter

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import ru.orangepigment.lox.ast.{Binary, BooleanLiteral, Grouping, Literal, NilLiteral, NumberLiteral, StringLiteral, Unary}
import ru.orangepigment.lox.errors.RuntimeError
import ru.orangepigment.lox.gen.*
import ru.orangepigment.lox.scanning.*

class InterpreterSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  "Interpreter.evaluate" should "correctly evaluate provided expression" in {
    val expression =
      Binary(
        Unary(
          Minus("-", LineNum(1)),
          NumberLiteral(123)
        ),
        Star("*", LineNum(1)),
        Grouping(NumberLiteral(45.67))
      )

    Interpreter.evaluate(expression) shouldEqual Right(Option(-5617.41))
  }

  it should "eval subtraction correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          Minus("-", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 - d2))
    }
  }

  it should "eval addition correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          Plus("+", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 + d2))
    }
  }

  it should "return runtime error on addition of non-numbers or non-strings" in {
    val gen = Gen.oneOf(
      Gen.zip(genNonNumberNorString, genNonNumberNorString),
      Gen.zip(genNumberLiteral, genNonNumberNorString),
      Gen.zip(genNonNumberNorString, genNumberLiteral),
      Gen.zip(genStringLiteral, genNonNumberNorString),
      Gen.zip(genNonNumberNorString, genStringLiteral)
    )
    forAll(gen) { (lit1, lit2) =>
      val expression =
        Binary(
          lit1,
          Plus("+", LineNum(1)),
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Left(RuntimeError(Plus("+", LineNum(1)), "Operands must be two numbers or two strings."))
    }
  }

  it should "eval concatenation correctly for strings" in {
    forAll { (s1: String, s2: String) =>
      val expression =
        Binary(
          StringLiteral(s1),
          Plus("+", LineNum(1)),
          StringLiteral(s2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(s1 + s2))
    }
  }

  it should "eval multiplication correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          Star("*", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 * d2))
    }
  }

  it should "eval division correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          Slash("/", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 / d2))
    }
  }

  it should "return runtime error on subtraction, multiplication or division of non-numbers" in {
    val operatorsGen = Gen.oneOf(
      Minus("-", LineNum(1)),
      Star("*", LineNum(1)),
      Slash("/", LineNum(1))
    )
    val literalsGen = Gen.oneOf(
      Gen.zip(genNonNumber, genNonNumber),
      Gen.zip(genNumberLiteral, genNonNumber),
      Gen.zip(genNonNumber, genNumberLiteral),
    )
    val gen = Gen.zip(operatorsGen, literalsGen)
    forAll(gen) { case (operator, (lit1, lit2)) =>
      val expression =
        Binary(
          lit1,
          operator,
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Left(RuntimeError(operator, "Operands must be two numbers."))
    }
  }

  it should "eval > correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          Greater(">", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 > d2))
    }
  }

  it should "eval >= relation correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          GreaterEqual(">=", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 >= d2))
    }
  }

  it should "eval < relation correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          Less("<", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 < d2))
    }
  }

  it should "eval <= relation correctly for numbers" in {
    forAll { (d1: Double, d2: Double) =>
      val expression =
        Binary(
          NumberLiteral(d1),
          LessEqual("<=", LineNum(1)),
          NumberLiteral(d2)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(d1 <= d2))
    }
  }

  it should "return runtime error on >, >=, <, <= checks of non-numbers" in {
    val operatorsGen = Gen.oneOf(
      Greater(">", LineNum(1)),
      GreaterEqual(">=", LineNum(1)),
      Less("<", LineNum(1)),
      LessEqual("<=", LineNum(1))
    )
    val gen = Gen.zip(operatorsGen, genNonNumber, genNonNumber)
    forAll(gen) { case (operator, lit1, lit2) =>
      val expression =
        Binary(
          lit1,
          operator,
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Left(RuntimeError(operator, "Operands must be two numbers."))
    }
  }

  it should "eval negation correctly for numbers" in {
    forAll { (d: Double) =>
      val expression =
        Unary(
          Minus("-", LineNum(1)),
          NumberLiteral(d)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(-d))
    }
  }

  it should "return runtime error on negation of non-numbers" in {
    forAll(genNonNumber) { (lit: Literal) =>
      val expression =
        Unary(
          Minus("-", LineNum(1)),
          lit
        )

      Interpreter.evaluate(expression) shouldEqual Left(RuntimeError(Minus("-", LineNum(1)), "Operand must be a number."))
    }
  }

  it should "eval logical negation correctly for booleans" in {
    forAll { (b: Boolean) =>
      val expression =
        Unary(
          Bang("!", LineNum(1)),
          BooleanLiteral(b)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(!b))
    }
  }

  it should "eval logical negation correctly for numbers" in {
    forAll { (d: Double) =>
      val expression =
        Unary(
          Bang("!", LineNum(1)),
          NumberLiteral(d)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(false))
    }
  }

  it should "eval logical negation correctly for strings" in {
    forAll { (s: String) =>
      val expression =
        Unary(
          Bang("!", LineNum(1)),
          StringLiteral(s)
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(false))
    }
  }

  it should "eval logical negation correctly for nil" in {
    val expression =
      Unary(
        Bang("!", LineNum(1)),
        NilLiteral
      )

    Interpreter.evaluate(expression) shouldEqual Right(Option(true))
  }

  it should "check any non-nil literal equality to nil correctly" in {
    val gen = Gen.oneOf(
      Gen.zip(genNonNil, genNil),
      Gen.zip(genNil, genNonNil)
    )
    forAll(gen) { (lit1, lit2) =>
      val expression =
        Binary(
          lit1,
          EqualEqual("==", LineNum(1)),
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(false))
    }
  }

  it should "check any non-nil literal non-equality to nil correctly" in {
    val gen = Gen.oneOf(
      Gen.zip(genNonNil, genNil),
      Gen.zip(genNil, genNonNil)
    )
    forAll(gen) { (lit1, lit2) =>
      val expression =
        Binary(
          lit1,
          BangEqual("!=", LineNum(1)),
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(true))
    }
  }

  it should "check nil equality correctly" in {
    val expression =
      Binary(
        NilLiteral,
        EqualEqual("==", LineNum(1)),
        NilLiteral
      )

    Interpreter.evaluate(expression) shouldEqual Right(Option(true))
  }

  it should "check nil non-equality correctly" in {
    val expression =
      Binary(
        NilLiteral,
        BangEqual("!=", LineNum(1)),
        NilLiteral
      )

    Interpreter.evaluate(expression) shouldEqual Right(Option(false))
  }

  it should "check equality of literals of different types correctly" in {
    val gen = Gen.oneOf(
      Gen.zip(genNumberLiteral, genStringLiteral),
      Gen.zip(genNumberLiteral, genBooleanLiteral),
      Gen.zip(genStringLiteral, genNumberLiteral),
      Gen.zip(genStringLiteral, genBooleanLiteral),
      Gen.zip(genBooleanLiteral, genNumberLiteral),
      Gen.zip(genBooleanLiteral, genStringLiteral),
    )
    forAll(gen) { (lit1, lit2) =>
      val expression =
        Binary(
          lit1,
          EqualEqual("==", LineNum(1)),
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(false))
    }
  }

  it should "check non-equality of literals of different types correctly" in {
    val gen = Gen.oneOf(
      Gen.zip(genNumberLiteral, genStringLiteral),
      Gen.zip(genNumberLiteral, genBooleanLiteral),
      Gen.zip(genStringLiteral, genNumberLiteral),
      Gen.zip(genStringLiteral, genBooleanLiteral),
      Gen.zip(genBooleanLiteral, genNumberLiteral),
      Gen.zip(genBooleanLiteral, genStringLiteral),
    )
    forAll(gen) { (lit1, lit2) =>
      val expression =
        Binary(
          lit1,
          BangEqual("!=", LineNum(1)),
          lit2
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(true))
    }
  }

  it should "check equality of same literals correctly" in {
    val gen = Gen.oneOf(
      genNumberLiteral,
      genStringLiteral,
      genBooleanLiteral
    )
    forAll(gen) { lit =>
      val expression =
        Binary(
          lit,
          EqualEqual("==", LineNum(1)),
          lit
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(true))
    }
  }

  it should "check non-equality of same literals correctly" in {
    val gen = Gen.oneOf(
      genNumberLiteral,
      genStringLiteral,
      genBooleanLiteral
    )
    forAll(gen) { lit =>
      val expression =
        Binary(
          lit,
          BangEqual("!=", LineNum(1)),
          lit
        )

      Interpreter.evaluate(expression) shouldEqual Right(Option(false))
    }
  }

  /*"Interpreter.interpretAndStringify" should "format result correctly" in {
    forAll(Gen.oneOf(genStringLiteral, genBooleanLiteral)) { lit =>
      Interpreter.interpretAndStringify(lit) shouldEqual Right(lit.literal.toString)
    }
  }

  // It is unpredictable, how long numbers will be formatted. Plus precision loss.
  it should "format whole numbers correctly" ignore {
    forAll { (l: Long) =>
      Interpreter.interpretAndStringify(NumberLiteral(l)) shouldEqual Right(l.toString)
    }
  }

  it should "format nil correctly" in {
    Interpreter.interpretAndStringify(NilLiteral) shouldEqual Right("nil")
  }*/
}
