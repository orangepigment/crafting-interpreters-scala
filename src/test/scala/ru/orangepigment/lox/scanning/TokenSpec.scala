package ru.orangepigment.lox.scanning

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.scanning.TokenType._

class TokenSpec extends AnyFlatSpec with Matchers {
  "Token.toString" should "be formatted correctly" in {
    GreaterEqual(">=", LineNum(33)).toString shouldEqual "L33: GreaterEqual >="

    IdentifierToken("aBcdEf", "aBcdEf", LineNum(27)).toString shouldEqual
      "L27: IdentifierToken aBcdEf aBcdEf"

    StringToken("\"str\"", "str", LineNum(6)).toString shouldEqual
      "L6: StringToken \"str\" str"
  }
}
