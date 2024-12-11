package ru.orangepigment.lox

import ru.orangepigment.lox.ast.AstPrinter
import ru.orangepigment.lox.errors.*
import ru.orangepigment.lox.interpreter.Interpreter
import ru.orangepigment.lox.parser.Parser
import ru.orangepigment.lox.scanning.{EOF, LineNum, Scanner}

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
    run(source).left.foreach {
      case _: ScannerError | _: ParserError => sys.exit(65)
      case _: RuntimeError => sys.exit(70)
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

  private def run(source: String): Either[LoxError, Unit] = {
    (for {
      tokens <- Scanner.scanTokens(source)
      expr <- Parser.parse(tokens.toArray)
      //_ <- Right(AstPrinter.print(expr))
      _ <- Interpreter.interpretAndStringify(expr).map(println)
    } yield ()).tapError(reportError)
  }

  private def reportError(error: LoxError): Unit = {
    error match
      case ScannerError(line, message) => report(line, "", message)
      case ParserError(token, message) =>
        token match {
          case EOF(lexeme, line) => report(line, " at end", message)
          case _ => report(token.line, " at '" + token.lexeme + "'", message)
        }
      case RuntimeError(token, message) => report(token.line, "", message)
  }

  private def report(line: LineNum, where: String, message: String): Unit =
    println(s"[line ${line.int}] Error$where: $message")

}
