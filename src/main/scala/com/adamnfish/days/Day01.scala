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
      totalDistance = sortColumns(intPairs)
        .map(distance.tupled)
        .sum
    yield totalDistance

  def sortColumns(intPairs: List[(Int, Int)]): List[(Int, Int)] =
    val (l, r) = intPairs.unzip
    l.sortBy(identity).zip(r.sortBy(identity))

  def distance(l: Int, r: Int): Int =
    math.abs(l - r)

  object Parser:
    import fastparse.*, SingleLineWhitespace.*

    def parseLine(line: String): IO[(Int, Int)] =
      parse(line, lineParser(using _)).intoF

    def lineParser[$: P]: P[(Int, Int)] =
      P(Parsing.integer ~ Parsing.integer)
