package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import fs2.{Compiler, Stream}
import com.adamnfish.Parsing.*
import com.adamnfish.days.Day06.Guard
import com.adamnfish.{Parsing, Tools}

object Day06:
  def part1(inputFile: String) =
    for
      (mapEdges, obstacles, guardOpt) <- getMappedArea(
        Tools
          .inputLines("6", inputFile)
      )
      guard <- IO.fromOption(guardOpt)(new RuntimeException("No guard found"))
      (_, visited) = advanceGuardToEnd(guard, obstacles, mapEdges)
    yield visited.size

  def part2(inputFile: String) =
    for
      (mapEdges, obstacles, guardOpt) <- getMappedArea(
        Tools
          .inputLines("6", inputFile)
      )
      guard <- IO.fromOption(guardOpt)(new RuntimeException("No guard found"))
      (_, visited) = advanceGuardToEnd(guard, obstacles, mapEdges)
      // "visited" is the set of all the places a relevant obstacle can be placed
      looping = visited.map { newObstacle =>
        val updatedObstacles = obstacles.copy(
          byColumn = obstacles.byColumn.updatedWith(newObstacle.col) {
            case Some(existing) =>
              Some(existing + newObstacle)
            case None =>
              Some(Set(newObstacle))
          },
          byRow = obstacles.byRow.updatedWith(newObstacle.row) {
            case Some(existing) =>
              Some(existing + newObstacle)
            case None =>
              Some(Set(newObstacle))
          }
        )
        advanceGuardToEnd(guard, updatedObstacles, mapEdges)
      }
    yield
      // all the adjusted maps that provided a loop
      looping.count(_._1)

  def getMappedArea(
      input: Stream[IO, String]
  ): IO[(MapEdges, Obstacles, Option[Guard])] =
    for
      firstRow <- input.take(1).compile.lastOrError
      colsCount = firstRow.length
      (rowsCount, obstacles, guardOpt) <- input
        .filter(_.nonEmpty)
        .map(parseRow)
        .scan[(Int, Obstacles, Option[Guard])]((0, Obstacles.empty, None))(
          accumulateMappedArea
        )
        .compile
        .lastOrError
    yield (MapEdges(0, rowsCount, 0, colsCount), obstacles, guardOpt)

  // (0, 0) is top left in this system
  case class Coord(col: Int, row: Int)
  enum Direction:
    case Up, Down, Left, Right
  case class Guard(facing: Direction, position: Coord)
  case class Obstacles(
      byColumn: Map[Int, Set[Coord]],
      byRow: Map[Int, Set[Coord]]
  )
  object Obstacles:
    def empty: Obstacles = Obstacles(Map.empty, Map.empty)
  case class MapEdges(minRow: Int, maxRow: Int, minCol: Int, maxCol: Int)

  // parse an individual row
  // at this point we don't know which row we're in, so we can't create full coordinates yet
  def parseRow(line: String): (Set[Int], Option[(Direction, Int)]) =
    line.zipWithIndex.foldLeft((Set.empty, None)) {
      case ((obstacles, guard), (char, col)) =>
        char match
          case '#' => (obstacles + col, guard)
          case '^' => (obstacles, Some((Direction.Up, col)))
          case 'v' => (obstacles, Some((Direction.Down, col)))
          case '<' => (obstacles, Some((Direction.Left, col)))
          case '>' => (obstacles, Some((Direction.Right, col)))
          case _   => (obstacles, guard)
    }

  // this lets us add row numbers to our partial data
  // we also create indexed versions of our obstacle data so we can efficiently query it later
  def accumulateMappedArea(
      acc: (Int, Obstacles, Option[Guard]),
      currentRow: (Set[Int], Option[(Direction, Int)])
  ): (Int, Obstacles, Option[Guard]) =
    val (rowIndex, accObstacles, accGuardOpt) = acc
    val (obstacleCols, guardOpt) = currentRow
    // add the guard if we don't already have it, and it is in this row
    val guard = accGuardOpt.orElse(
      guardOpt.map { (direction, col) =>
        Guard(direction, Coord(col, rowIndex))
      }
    )
    // add all the obstacles in this row
    val obstacleCoords = obstacleCols.map { col =>
      Coord(col, rowIndex)
    }
    val obstaclesWithColumns = obstacleCoords.foldLeft(accObstacles) {
      case (acc, obstacle) =>
        acc.copy(
          byColumn = acc.byColumn.updatedWith(obstacle.col) {
            case Some(columnObstacles) => Some(columnObstacles + obstacle)
            case None                  => Some(Set(obstacle))
          }
        )
    }
    val finalObstacles =
      obstaclesWithColumns.copy(
        byRow = obstaclesWithColumns.byRow + (rowIndex -> obstacleCoords)
      )
    (rowIndex + 1, finalObstacles, guard)

  // returns all the locations the guard visits
  def advanceGuardToEnd(
      startGuard: Guard,
      obstacles: Obstacles,
      mapEdges: MapEdges
  ): (Boolean, Set[Coord]) =
    def loop(guard: Guard, acc: Set[Guard]): (Boolean, Set[Guard]) =
      val (maybeCollision, finishCells) =
        guard.facing match {
          case Direction.Up =>
            (
              obstacles.byColumn
                .get(guard.position.col)
                // obstacles in the direction guard is facing
                .map(_.filter(_.row < guard.position.row))
                // the closest of the relevant obstacles
                .flatMap(_.maxByOption(_.row))
                // all cells between current position and the obstacle
                .map(obstacle =>
                  (
                    // new guard position
                    Guard(
                      Direction.Right,
                      Coord(guard.position.col, obstacle.row + 1)
                    ),
                    // cells that got added
                    (obstacle.row + 1)
                      .until(guard.position.row)
                      .toSet
                      .map(row =>
                        Guard(guard.facing, Coord(guard.position.col, row))
                      )
                  )
                ),
              (guard.position.row)
                .to(mapEdges.minRow + 1)
                .toSet
                .map(row => Guard(guard.facing, Coord(guard.position.col, row)))
            )
          case Direction.Down =>
            (
              obstacles.byColumn
                .get(guard.position.col)
                .map(_.filter(_.row > guard.position.row))
                .flatMap(_.minByOption(_.row))
                .map(obstacle =>
                  (
                    Guard(
                      Direction.Left,
                      Coord(guard.position.col, obstacle.row - 1)
                    ),
                    guard.position.row
                      .until(obstacle.row)
                      .toSet
                      .map(row =>
                        Guard(guard.facing, Coord(guard.position.col, row))
                      )
                  )
                ),
              guard.position.row
                .until(mapEdges.maxRow)
                .toSet
                .map(row => Guard(guard.facing, Coord(guard.position.col, row)))
            )
          case Direction.Left =>
            (
              obstacles.byRow
                .get(guard.position.row)
                .map(_.filter(_.col < guard.position.col))
                .flatMap(_.maxByOption(_.col))
                .map(obstacle =>
                  (
                    Guard(
                      Direction.Up,
                      Coord(obstacle.col + 1, guard.position.row)
                    ),
                    (obstacle.col + 1)
                      .until(guard.position.col)
                      .toSet
                      .map(col =>
                        Guard(guard.facing, Coord(col, guard.position.row))
                      )
                  )
                ),
              (mapEdges.minCol)
                .until(guard.position.col)
                .toSet
                .map(col => Guard(guard.facing, Coord(col, guard.position.row)))
            )
          case Direction.Right =>
            (
              obstacles.byRow
                .get(guard.position.row)
                .map(_.filter(_.col > guard.position.col))
                .flatMap(_.minByOption(_.col))
                .map(obstacle =>
                  (
                    Guard(
                      Direction.Down,
                      Coord(obstacle.col - 1, guard.position.row)
                    ),
                    guard.position.col
                      .until(obstacle.col)
                      .toSet
                      .map(col =>
                        Guard(guard.facing, Coord(col, guard.position.row))
                      )
                  )
                ),
              guard.position.col
                .until(mapEdges.maxCol)
                .toSet
                .map(col => Guard(guard.facing, Coord(col, guard.position.row)))
            )
        }

      maybeCollision match
        case Some((newGuard, newLocations)) =>
          // we hit a block, so we'll loop around and go again from the new location
          // but first check if any new locations have already been visited
          // if so, we have a loop!
          if ((newLocations - guard).intersect(acc).nonEmpty)
            (true, newLocations ++ acc)
          else
            loop(guard = newGuard, acc = newLocations ++ acc)
        case None =>
          // we're finished, so just add the new locations
          // if we finished then there's no loop!
          (false, acc ++ finishCells)

    val (isLoop, guardPositions) = loop(startGuard, Set.empty)
    (isLoop, guardPositions.map(_.position))

  def advanceGuardToLoop(
      startGuard: Guard,
      obstacles: Obstacles,
      mapEdges: MapEdges
  ): Boolean =
    ???

  def visualise(
      edges: MapEdges,
      obstacles: Obstacles,
      visited: Set[Coord],
      guardOpt: Option[Guard]
  ): String =
    (0 until edges.maxRow)
      .map { row =>
        val chars = for
          col <- 0 until edges.maxCol
          coord = Coord(col, row)
          isObstacle = obstacles.byColumn
            .get(col)
            .flatMap(_.find(_ == coord))
            .isDefined
          isVisited = visited.contains(coord)
        yield
          if (isVisited && isObstacle) '?'
          else if (isVisited) '+'
          else if (isObstacle) '#'
          else if (guardOpt.exists(_.position == coord))
            guardOpt
              .map {
                case Guard(Direction.Up, _)    => '^'
                case Guard(Direction.Down, _)  => 'v'
                case Guard(Direction.Left, _)  => '<'
                case Guard(Direction.Right, _) => '>'
              }
              .getOrElse('?')
          else '.'
        chars.mkString
      }
      .mkString("\n")
