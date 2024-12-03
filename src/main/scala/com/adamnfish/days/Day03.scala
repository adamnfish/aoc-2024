package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Tools
import com.adamnfish.Parsing.*

object Day03:
  def part1(inputFile: String) =
    Tools
      .inputLines("3", inputFile)
      .evalMap(Part1.Parser.parseLine[IO])
      .compile
      .toList
      .map { parsedLines =>
        parsedLines.flatten
          .map(m => m._1 * m._2)
          .sum
      }

  def part2(inputFile: String) =
    Tools
      .inputLines("3", inputFile)
      .evalMap(Part2.Parser.parseLine[IO])
      .compile
      .toList
      .map { parsedLines =>
        val operations = parsedLines.flatten
        Part2.runOperations(operations)
      }

  object Part1:
    object Parser:
      import fastparse.*
      import MultiLineWhitespace.*

      def parseLine[F[_]: MonadThrow](line: String): F[List[(Int, Int)]] =
        parse(line, lineParser(using _)).intoF

      private def lineParser[$: P]: P[List[(Int, Int)]] =
        P(
          (!multParser ~ AnyChar).rep ~
            multParser.rep(sep = (!multParser ~ AnyChar).rep)
        ).map(_.toList)

      private def multParser[$: P]: P[(Int, Int)] =
        P("mul(" ~ integer ~ "," ~ integer ~ ")")

  object Part2:
    enum Operation:
      case Do()
      case Dont()
      case Mult(n1: Int, n2: Int)

    def runOperations(operations: List[Operation]): Int =
      val (result, _) = operations.foldLeft((0, true)) {
        case ((total, _), Operation.Do()) =>
          (total, true)
        case ((total, _), Operation.Dont()) =>
          (total, false)
        case ((total, false), Operation.Mult(_, _)) =>
          (total, false)
        case ((total, true), Operation.Mult(n1, n2)) =>
          (total + n1 * n2, true)
      }
      result

    object Parser:
      import fastparse.*
      import MultiLineWhitespace.*

      def parseLine[F[_]: MonadThrow](line: String): F[List[Operation]] =
        parse(line, lineParser(using _)).intoF

      private def lineParser[$: P]: P[List[Operation]] =
        P(
          (!operationParser ~ AnyChar).rep ~
            operationParser.rep(sep = (!operationParser ~ AnyChar).rep)
        ).map(_.toList)

      private def operationParser[$: P]: P[Operation] =
        P(multParser | doParser | dontParser)

      private def multParser[$: P]: P[Operation.Mult] =
        P("mul(" ~ integer ~ "," ~ integer ~ ")").map { case (n1, n2) =>
          Operation.Mult(n1, n2)
        }

      private def doParser[$: P]: P[Operation.Do] =
        P("do()").map(_ => Operation.Do())

      private def dontParser[$: P]: P[Operation.Dont] =
        P("don't()").map(_ => Operation.Dont())
