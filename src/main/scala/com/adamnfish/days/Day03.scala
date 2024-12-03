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
      .evalMap(Parser.parseLine[IO])
      .compile
      .toList
      .map { parsedLines =>
        parsedLines.flatten
          .map(m => m._1 * m._2)
          .sum
      }

  def part2(inputFile: String) =
    Tools.inputLines("3", inputFile).compile.lastOrError

  object Parser:
    import fastparse.*

    def parseLine[F[_]: MonadThrow](line: String): F[List[(Int, Int)]] =
      parse(line, lineParser(using _)).intoF

    private def lineParser[$: P]: P[List[(Int, Int)]] =
      import MultiLineWhitespace.*
      P(
        (!multParser ~ AnyChar).rep ~
          multParser.rep(sep = (!multParser ~ AnyChar).rep)
      ).map(_.toList)

    private def multParser[$: P]: P[(Int, Int)] =
      import NoWhitespace.*
      P("mul(" ~ integer ~ "," ~ integer ~ ")")
