package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.{Parsing, Tools}
import com.adamnfish.Parsing.intoF

object Day01:
  def part1(inputFile: String) =
    for
      intPairs <- Tools
        .inputLines("1", inputFile)
        .filter(_.nonEmpty)
        .evalMap(Parser.parseLine)
        .compile
        .toList
      totalDistance = Part1
        .sortColumns(intPairs)
        .map(Part1.distance.tupled)
        .sum
    yield totalDistance

  def part2(inputFile: String) =
    for
      intPairs <- Tools
        .inputLines("1", inputFile)
        .filter(_.nonEmpty)
        .evalMap(Parser.parseLine)
        .compile
        .toList
      similarityScore = Part2
        .groupByCount(intPairs)
        .map { case (n, count) => n * count }
        .sum
    yield similarityScore

  object Part1:
    def sortColumns(intPairs: List[(Int, Int)]): List[(Int, Int)] =
      val (l, r) = intPairs.unzip
      l.sortBy(identity).zip(r.sortBy(identity))

    def distance(l: Int, r: Int): Int =
      math.abs(l - r)

  object Part2:
    def groupByCount(intPairs: List[(Int, Int)]): List[(Int, Int)] =
      val (l, r) = intPairs.unzip
      l.foldRight(Nil) { case (n, acc) =>
        (n, r.count(_ == n)) :: acc
      }

  object Parser:
    import fastparse.*, SingleLineWhitespace.*

    def parseLine(line: String): IO[(Int, Int)] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[(Int, Int)] =
      P(Parsing.integer ~ Parsing.integer)
