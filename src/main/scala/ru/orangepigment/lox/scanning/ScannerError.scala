package ru.orangepigment.lox.scanning

final case class ScannerError(line: LineNum, message: String)
