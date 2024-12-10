package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Parsing.*
import com.adamnfish.{Parsing, Tools}

object Day07:
  def part1(inputFile: String) =
    Tools
      .inputLines("7", inputFile)
      .filter(_.nonEmpty)
      .evalMap(Parser.parseLine[IO])
      .filter(Part1.canBeSolved)
      .map(_.answer)
      .scan(0L)(_ + _)
      .compile
      .lastOrError

  def part2(inputFile: String) =
    Tools
      .inputLines("7", inputFile)
      .filter(_.nonEmpty)
      .evalMap(Parser.parseLine[IO])
      .filter(Part2.canBeSolved)
      .map(_.answer)
      .scan(0L)(_ + _)
      .compile
      .lastOrError

  object Part1:
    def canBeSolved(equation: Equation): Boolean =
      val (solved, _) = equation.operands.foldLeft((false, Seq(0L))) {
        case ((solved, totals), operand) =>
          val newTotals = totals.flatMap { total =>
            // add and multiply by the current operand
            Seq(total + operand, total * operand).filter(_ <= equation.answer)
          }
          if (newTotals.contains(equation.answer))
            (true, newTotals)
          else
            (false, newTotals)
      }
      solved

  object Part2:
    def canBeSolved(equation: Equation): Boolean =
      val (solved, _) = equation.operands.foldLeft((false, Seq(0L))) {
        case ((solved, totals), operand) =>
          val newTotals = totals.flatMap { total =>
            // add and multiply by the current operand
            Seq(total + operand, total * operand, s"${total}${operand}".toLong)
              .filter(_ <= equation.answer)
          }
          if (newTotals.contains(equation.answer))
            (true, newTotals)
          else
            (false, newTotals)
      }
      solved

  case class Equation(answer: Long, operands: Seq[Long])

  object Parser:
    import fastparse.*
    import NoWhitespace.*

    def parseLine[F[_]: MonadThrow](line: String): F[Equation] =
      parse(line, lineParser(using _)).intoF

    private def lineParser[$: P]: P[Equation] =
      P(long ~ ":" ~ paddedLong.rep).map { case (answer, operands) =>
        Equation(answer, operands)
      }
