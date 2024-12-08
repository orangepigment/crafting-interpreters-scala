package ru.orangepigment.lox

import ru.orangepigment.lox.ast.AstPrinter
import ru.orangepigment.lox.parser.{Parser, ParserError}
import ru.orangepigment.lox.scanning.{And, Bang, BangEqual, Comma, Dot, EOF, Else, Equal, EqualEqual, False, For, Fun, Greater, GreaterEqual, If, LeftBrace, LeftParen, Less, LessEqual, LineNum, Minus, Nil, Or, Plus, Print, Return, RightBrace, RightParen, Scanner, ScannerError, Semicolon, Slash, Star, Super, This, TokenLiteral, True, Var, While}

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.io.StdIn

object Lox {
  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: scalox [script]")
      sys.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  private def runFile(path: String): Unit = {
    val source = new String(Files.readAllBytes(Paths.get(path)), Charset.defaultCharset())
    if (!run(source)) {
      sys.exit(65)
    }
  }

  @tailrec
  private def runPrompt(): Unit = {
    val line = StdIn.readLine()
    if (line != null) {
      run(line)
      runPrompt()
    }
  }

  private def run(source: String): Boolean = {
    Scanner.scanTokens(source) match
      case Left(error) => reportError(error)
      case Right(tokens) =>
        tokens.foreach(println)
        Parser.parse(tokens.toArray) match
          case Left(error) => reportError(error)
          case Right(expr) => AstPrinter.print(expr)
        true
  }

  // ToDo: make a single ADT for errors? Or do not mix domains?
  private def reportError(error: ScannerError): Boolean =
    report(error.line, "", error.message)

  private def reportError(error: ParserError): Boolean =
    error.token match {
      case EOF(lexeme, line) => report(line, " at end", error.message)
      case _ => report(error.token.line, " at '" + error.token.lexeme + "'", error.message)
    }
  private def report(line: LineNum, where: String, message: String): Boolean = {
    println(s"[line ${line.int}] Error$where: $message")
    false
  }

}
