package ru.orangepigment.lox.scanning

import ru.orangepigment.lox.scanning.TokenType.*

import scala.annotation.tailrec


object Scanner {

  def scanTokens(source: String): Either[ScannerError, List[Token]] = {
    @tailrec
    def scanLoop(
                  line: LineNum,
                  current: Position,
                  tokens: List[Token]
                ): Either[ScannerError, List[Token]] = {
      if (current >= source.length) {
        Right((Token(EOF, "", None, line) +: tokens).reverse)
      } else {
        source(current.int) match {
          // Simple one-char tokens
          case char if oneCharTokens.keySet.contains(char) =>
            val tokenType = oneCharTokens(char)
            val (i, token) = getToken(source, line, current, current, tokenType)
            scanLoop(line, i, token +: tokens)
          // One-two char operators
          case char if oneOrTwoCharTokens.contains(char) =>
            val (expectedSecondChar, oneCharTokenType, twoCharTokenType) = oneOrTwoCharTokens(char)
            val (i, token) = oneOrTwoCharToken(source, line, current, expectedSecondChar, oneCharTokenType, twoCharTokenType)
            scanLoop(line, i, token +: tokens)
          // Comments
          case '/' =>
            val posToCheck = current + 1
            // One-line comment
            if (matchChar(source, '/', posToCheck)) {
              // A comment goes until the end of the line
              val endOfCommentPos = skipUntilTheEOL(source, posToCheck)
              scanLoop(line, endOfCommentPos, tokens)
            } else if (matchChar(source, '*', posToCheck)) { // Multi-line comment
              multiLineComment(source, line, posToCheck + 1) match
                case Left(error) => Left(error)
                case Right((endOfCommentLine, endOfCommentPos)) =>
                  scanLoop(endOfCommentLine, endOfCommentPos, tokens)
            } else {
              val (i, token) = getToken(source, line, current, current, SLASH)
              scanLoop(line, i, token +: tokens)
            }
          case ' ' | '\r' | '\t' => scanLoop(line, current + 1, tokens)
          case '\n' => scanLoop(line + 1, current + 1, tokens)
          case '"' =>
            string(source, line, current) match
              case Left(error) => Left(error) // Change the right type
              case Right((newLine, i, token)) =>
                scanLoop(newLine, i, token +: tokens)
          case char if char.isDigit =>
            val (i, token) = number(source, line, current)
            scanLoop(line, i, token +: tokens)
          case char if char.isLetter || char == '_' =>
            val (i, token) = identifier(source, line, current)
            scanLoop(line, i, token +: tokens)
          case unexpectedChar => Left(ScannerError(line, s"Unexpected character '$unexpectedChar'."))
        }
      }
    }

    scanLoop(LineNum(1), Position(0), List.empty)
  }

  private def getToken(
                        source: String,
                        line: LineNum,
                        start: Position,
                        end: Position,
                        tokenType: TokenType
                      ): (Position, Token) = {
    val next = end + 1
    val token =
      tokenType match {
        case STRING =>
          // Trim the surrounding quotes
          val raw = source.substring(start.int + 1, next.int - 1)
          Token(tokenType, s"\"$raw\"", Option(StringLiteral(raw)), line)

        case NUMBER =>
          val raw = source.substring(start.int, next.int)
          Token(tokenType, raw, Option(NumberLiteral(raw.toDouble)), line)

        case IDENTIFIER =>
          val raw = source.substring(start.int, next.int)
          keywords.get(raw) match
            case Some(keyword) => Token(keyword, raw, Option.empty[TokenLiteral], line)
            case None =>
              Token(tokenType, raw, Option(IdentifierLiteral(raw)), line)

        case _ =>
          val raw = source.substring(start.int, next.int)
          Token(tokenType, raw, Option.empty[TokenLiteral], line)
      }

    next -> token
  }

  private def oneOrTwoCharToken(
                                 source: String,
                                 line: LineNum,
                                 start: Position,
                                 expectedSecondChar: Char,
                                 oneCharTokenType: TokenType,
                                 twoCharTokenType: TokenType
                               ): (Position, Token) = {
    val posToCheck = start + 1
    if (matchChar(source, expectedSecondChar, posToCheck))
      getToken(source, line, start, posToCheck, twoCharTokenType)
    else getToken(source, line, start, start, oneCharTokenType)
  }

  @tailrec
  private def skipUntilTheEOL(source: String, current: Position): Position = {
    if (peek(source, current) != '\n') skipUntilTheEOL(source, current + 1) else current
  }

  private def matchChar(source: String, expected: Char, current: Position): Boolean =
    current < source.length && source.charAt(current.int) == expected

  private def peek(source: String, current: Position): Char = {
    if (current >= source.length) '\n' else source.charAt(current.int)
  }

  private def peekNext(source: String, current: Position): Char = {
    if (current + 1 >= source.length) '\n' else source.charAt(current.int + 1)
  }

  // Line Pos Token
  private def string(source: String, startLine: LineNum, start: Position): Either[ScannerError, (LineNum, Position, Token)] = {
    @tailrec
    def stringScan(line: LineNum, current: Position): Either[ScannerError, (LineNum, Position, Token)] = {
      if (current >= source.length) {
        Left(ScannerError(line, "Unterminated string."))
      } else {
        peek(source, current) match {
          case '"' =>
            val (i, token) = getToken(source, startLine, start, current, STRING)
            Right((line, i, token))

          case '\n' => stringScan(line + 1, current + 1)
          case _ => stringScan(line, current + 1)
        }
      }
    }

    stringScan(startLine, start + 1)
  }

  private def number(source: String, line: LineNum, start: Position): (Position, Token) = {
    @tailrec
    def numberScan(current: Position): (Position, Token) = {
      peek(source, current) match {
        case char if char.isDigit => numberScan(current + 1)
        // Continue to the fractional part
        case '.' if peekNext(source, current).isDigit => numberScan(current + 1)
        // Current char is already not a part of digit
        case _ => getToken(source, line, start, current - 1, NUMBER)
      }
    }

    numberScan(start + 1)
  }

  private def identifier(source: String, line: LineNum, start: Position): (Position, Token) = {
    @tailrec
    def identifierScan(line: LineNum, current: Position): (Position, Token) = {
      val peeked = peek(source, current)
      if (peeked.isLetterOrDigit || peeked == '_') {
        identifierScan(line, current + 1)
      } else {
        getToken(source, line, start, current - 1, IDENTIFIER)
      }
    }

    identifierScan(line, start + 1)
  }

  @tailrec
  private def multiLineComment(source: String, line: LineNum, current: Position): Either[ScannerError, (LineNum, Position)] =
    if (current >= source.length) {
      Left(ScannerError(line, "Unterminated multi-line comment."))
    } else {
      source.charAt(current.int) match
        case '*' =>
          val posToCheck = current + 1
          if (matchChar(source, '/', posToCheck)) {
            Right((line, posToCheck + 1))
          } else {
            multiLineComment(source, line, current + 1)
          }
        case '\n' => multiLineComment(source, line + 1, current + 1)
        case _ => multiLineComment(source, line, current + 1)
    }
}
