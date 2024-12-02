package ru.orangepigment.lox.scanning

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.orangepigment.lox.scanning.TokenType._

class TokenSpec extends AnyFlatSpec with Matchers {
  "Token.toString" should "be formatted correctly" in {
    Token(GREATER_EQUAL, ">=", None, LineNum(33)).toString shouldEqual "L33: GREATER_EQUAL >="

    Token(IDENTIFIER, "aBcdEf", Some(IdentifierLiteral("aBcdEf")), LineNum(27)).toString shouldEqual
      "L27: IDENTIFIER aBcdEf IdentifierLiteral(aBcdEf)"

    Token(STRING, "\"str\"", Some(StringLiteral("str")), LineNum(6)).toString shouldEqual
      "L6: STRING \"str\" StringLiteral(str)"
  }
}
