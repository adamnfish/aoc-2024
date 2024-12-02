package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Tools
import com.adamnfish.Parsing
import com.adamnfish.Parsing.intoF
import com.adamnfish.days.Day01.Parser.lineParser
import com.adamnfish.days.Day02.DirectionChange.{Decreasing, Increasing}

import scala.annotation.tailrec

object Day02:
  def part1(inputFile: String) =
    Tools
      .inputLines("2", inputFile)
      .filter(_.nonEmpty)
      .evalMap(Parser.parseLine[IO])
      .evalMap(gradualChange[IO])
      .collect { case Some(_) =>
        1
      }
      .scan(0)(_ + _)
      .compile
      .lastOrError

  def part2(inputFile: String) =
    Tools.inputLines("2", inputFile).compile.lastOrError

  enum DirectionChange:
    case Increasing
    case Decreasing

  def gradualChange[F[_]: MonadThrow](
      ints: List[Int]
  ): F[Option[DirectionChange]] =
    @tailrec
    def loop(
        directionChange: DirectionChange,
        prev: Int,
        remainder: List[Int]
    ): Option[DirectionChange] =
      remainder match
        case Nil =>
          Some(directionChange)
        case next :: tail =>
          if (intWithin(prev, next, 3, directionChange))
            loop(directionChange, next, tail)
          else None

    ints match {
      case first :: second :: _ =>
        val result =
          if (first > second) loop(Decreasing, first, ints.tail)
          else if (first < second) loop(Increasing, first, ints.tail)
          else None
        MonadThrow[F].pure(result)

      case _ =>
        MonadThrow[F].raiseError(
          RuntimeException(
            s"Need at least 2 ints to tell direction, got ${ints.mkString(",")}"
          )
        )
    }

  def intWithin(
      prev: Int,
      next: Int,
      maxDiff: Int,
      directionChange: DirectionChange
  ): Boolean =
    if (directionChange == DirectionChange.Increasing && prev < next)
      next - prev <= maxDiff
    else if (directionChange == DirectionChange.Decreasing && prev > next)
      prev - next <= maxDiff
    else false

  object Parser:
    import fastparse.*
    import SingleLineWhitespace.*

    def parseLine[F[_]: MonadThrow](line: String): F[List[Int]] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[List[Int]] =
      P(Parsing.integer.rep).map(_.toList)
