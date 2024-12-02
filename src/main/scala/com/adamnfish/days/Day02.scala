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
      .evalMap(Part1.gradualChange[IO])
      .collect { case Some(_) =>
        1
      }
      .scan(0)(_ + _)
      .compile
      .lastOrError

  def part2(inputFile: String) =
    Tools
      .inputLines("2", inputFile)
      .filter(_.nonEmpty)
      .evalMap(Parser.parseLine[IO])
      .evalMap(Part2.gradualChange[IO])
      .collect { case Some(_) =>
        1
      }
      .scan(0)(_ + _)
      .compile
      .lastOrError

  enum DirectionChange:
    case Increasing
    case Decreasing

  object Part1:
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

  object Part2:
    // gets all sublists with a single element removed
    def removals(ints: List[Int]): List[List[Int]] =
      val length = ints.length
      0.until(length).toList.map { indexToDrop =>
        ints.slice(0, indexToDrop) ++ ints
          .slice(indexToDrop + 1, indexToDrop + 1 + length)
      }

    // first try the raw input, then try with all combinations of one element removed
    def gradualChange[F[_]: MonadThrow](
        ints: List[Int]
    ): F[Option[DirectionChange]] =
      Part1.gradualChange(ints).flatMap {
        case None =>
          removals(ints)
            .traverse(Part1.gradualChange)
            .map(_.find(_.isDefined).flatten)
        case Some(direction) =>
          MonadThrow[F].pure(Some(direction))
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
