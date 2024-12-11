package com.adamnfish.days

import cats.*
import cats.effect.IO
import com.adamnfish.Tools
import fs2.Stream

import scala.Console.*
import scala.annotation.tailrec

object Day08:
  def part1(inputFile: String) =
    for
      (mapSize, antennas) <- getMappedArea(
        Tools
          .inputLines("8", inputFile)
      )
      antinodes = antennas.view.mapValues(Part1.findAntinodes(_, mapSize))
      antinodePositions = antinodes.values.toSet.flatten
    yield antinodePositions.size

  def part2(inputFile: String) =
    for
      (mapSize, antennas) <- getMappedArea(
        Tools
          .inputLines("8", inputFile)
      )
      antinodes = antennas.view.mapValues(Part2.findAntinodes(_, mapSize))
      antinodePositions = antinodes.values.toSet.flatten
    yield antinodePositions.size

  object Part1:
    def findAntinodes(positions: Set[Coord], mapSize: MapSize): Set[Coord] =
      for
        p1 <- positions
        p2 <- positions
        if p1 != p2
        antinode = Coord((2 * p1.col) - p2.col, (2 * p1.row) - p2.row)
        if withinMapBounds(antinode, mapSize)
      yield antinode

  object Part2:
    def findAntinodes(positions: Set[Coord], mapSize: MapSize): Set[Coord] =
      for
        p1 <- positions
        p2 <- positions
        if p1 != p2
        colDelta = p1.col - p2.col
        rowDelta = p1.row - p2.row
        antinode <- translateWhileInBounds(p1, colDelta, rowDelta, mapSize)
        if withinMapBounds(antinode, mapSize)
      yield antinode

    // accumulate repeated applications of the given translation while it remains in bounds
    def translateWhileInBounds(
        position: Coord,
        deltaCol: Int,
        deltaRow: Int,
        mapSize: MapSize
    ): Set[Coord] =
      @tailrec
      def loop(last: Coord, acc: Set[Coord]): Set[Coord] =
        val next = Coord(last.col + deltaCol, last.row + deltaRow)
        if (withinMapBounds(next, mapSize)) loop(next, acc + next)
        else acc
      loop(position, Set(position))

  def withinMapBounds(coord: Coord, mapSize: MapSize): Boolean =
    coord.col >= 0 && coord.col < mapSize.cols &&
      coord.row >= 0 && coord.row < mapSize.rows

  def getMappedArea(
      input: Stream[IO, String]
  ): IO[(MapSize, Map[Char, Set[Coord]])] =
    for
      firstRow <- input.take(1).compile.lastOrError
      colsCount = firstRow.length
      (rowsCount, antennas) <- input
        .filter(_.nonEmpty)
        .map(parseLine)
        .scan[(Int, Map[Char, Set[Coord]])]((0, Map.empty))(
          accumulateMappedArea
        )
        .compile
        .lastOrError
    yield (MapSize(rowsCount, colsCount), antennas)

  case class Coord(col: Int, row: Int)
  case class MapSize(rows: Int, cols: Int) // assume 0 is the min in both cases

  def parseLine(line: String): Map[Char, Set[Int]] =
    line.zipWithIndex
      .filterNot { case (c, _) => c == '.' }
      .foldLeft(Map.empty[Char, Set[Int]]) { case (acc, (char, col)) =>
        Semigroup[Map[Char, Set[Int]]].combine(
          acc,
          Map(char -> Set(col))
        )
      }

  def accumulateMappedArea(
      acc: (Int, Map[Char, Set[Coord]]),
      currentRow: Map[Char, Set[Int]]
  ): (Int, Map[Char, Set[Coord]]) =
    val (rowIndex, accAntennas) = acc
    val currentRowCoords =
      currentRow.view.mapValues(_.map(col => Coord(col, rowIndex))).toMap
    (
      rowIndex + 1,
      Semigroup[Map[Char, Set[Coord]]].combine(accAntennas, currentRowCoords)
    )

  def visualiseMap(
      antennas: Map[Char, Set[Coord]],
      antinodes: Set[Coord],
      size: MapSize
  ): String =
    val rows = 0.until(size.rows).map { row =>
      val cols = 0.until(size.cols).map { col =>
        val here = Coord(col, row)
        val char = antennas
          .collectFirst { // there shouldn't be more than one antenna at a given location!
            case (char, coords) if coords.contains(here) => char
          }
          .getOrElse('.')
        if (antinodes.contains(here))
          // use a background colour for antinodes so they can share their location with an antenna
          s"${CYAN_B}${char}${RESET}"
        else s"$char"
      }
      cols.mkString
    }
    rows.mkString("\n")
