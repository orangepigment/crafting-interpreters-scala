package ru.orangepigment.lox

import ru.orangepigment.lox.scanning.{Scanner, ScannerError}

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

  private def run(source: String): Boolean =
    Scanner.scanTokens(source) match
      case Left(error) => reportError(error)
      case Right(tokens) =>
        tokens.foreach(println)
        true

  private def reportError(error: ScannerError): Boolean =
    report(error.line.int, "", error.message)

  private def report(line: Int, where: String, message: String): Boolean = {
    println(s"[line $line] Error$where: $message")
    false
  }

}
