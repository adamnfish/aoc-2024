package com.adamnfish.days

import fs2.Stream
import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Parsing.*
import com.adamnfish.{Parsing, Tools}
import Console.*

object Day08:
  def part1(inputFile: String) =
    for
      (mapSize, antennas) <- getMappedArea(
        Tools
          .inputLines("8", inputFile)
      )
      _ <- IO.println(visualiseMap(antennas, Set.empty, mapSize))
      _ <- IO.println("========")
      antinodes = antennas.view.mapValues(findAntinodes(_, mapSize))
      antinodePositions = antinodes.values.toSet.flatten
      _ <- IO.println(visualiseMap(antennas, antinodePositions, mapSize))
    yield antinodePositions.size

  def part2(inputFile: String) =
    Tools.inputLines("8", inputFile).compile.lastOrError

  def findAntinodes(positions: Set[Coord], mapSize: MapSize): Set[Coord] =
    for
      p1 <- positions
      p2 <- positions
      if p1 != p2
      maxCol = p1.col max p2.col
      minCol = p1.col min p2.col
      deltaCol = maxCol - minCol
      maxRow = p1.row max p2.row
      minRow = p1.row min p2.row
      deltaRow = maxRow - minRow
      antinode <- Set(
        // TODO: fix this - not quite this simple
        Coord(maxCol + deltaCol, maxRow + deltaRow),
        Coord(minCol - deltaCol, minRow - deltaRow)
      )
      if withinMapBounds(antinode, mapSize)
    yield antinode

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
      .foldLeft(Map.empty) { case (acc, (char, col)) =>
        acc.updatedWith(char) {
          case None =>
            Some(Set(col))
          case Some(existing) =>
            Some(existing + col)
        }
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
          // there shouldn't be more than one antenna here
          .collectFirst {
            case (char, coords) if coords.contains(here) => char
          }
          .getOrElse('.')
        if (antinodes.contains(here))
          s"${CYAN_B}${char}${RESET}"
        else s"$char"
      }
      cols.mkString
    }
    rows.mkString("\n")
