package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Parsing.*
import com.adamnfish.days.Day06.Guard
import com.adamnfish.{Parsing, Tools}

object Day06:
  def part1(inputFile: String) =
    for
      (_, obstacles, guardOpt) <- Tools
        .inputLines("6", inputFile)
        .filter(_.nonEmpty)
        .map(parseRow)
        .scan[(Int, Obstacles, Option[Guard])]((0, Obstacles.empty, None))(
          accumulateMappedArea
        )
        .compile
        .lastOrError
      // assume obstacles exist on the extreme edges - currently true!
      mapEdges = MapEdges(
        minRow = 0,
        maxRow = obstacles.byRow.keys.max,
        minCol = 0,
        maxCol = obstacles.byColumn.keys.max
      )
      _ <- IO.println(visualise(mapEdges, obstacles, Set.empty))
      guard <- IO.fromOption(guardOpt)(new RuntimeException("No guard found"))
      _ <- IO.println(s"obs: $obstacles, guard: $guard")
      visited = advanceGuardToEnd(guard, obstacles, mapEdges)
      _ <- IO.println(visited)
      _ <- IO.println(visualise(mapEdges, obstacles, visited))
    yield visited.size

  def part2(inputFile: String) =
    Tools.inputLines("6", inputFile).compile.lastOrError

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
  ): Set[Coord] =
    def loop(guard: Guard, acc: Set[Coord]): Set[Coord] =
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
                    guard.position.row
                      .until(obstacle.row)
                      .toSet
                      .map(row => Coord(guard.position.col, row))
                  )
                ),
              guard.position.row
                .until(mapEdges.minRow)
                .toSet
                .map(row => Coord(guard.position.col, row))
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
                      .map(row => Coord(guard.position.col, row))
                  )
                ),
              guard.position.row
                .until(mapEdges.maxRow)
                .toSet
                .map(row => Coord(guard.position.col, row))
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
                    guard.position.col
                      .until(obstacle.col)
                      .toSet
                      .map(col => Coord(col, guard.position.row))
                  )
                ),
              guard.position.col
                .until(mapEdges.minCol)
                .toSet
                .map(col => Coord(col, guard.position.row))
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
                      .map(col => Coord(col, guard.position.row))
                  )
                ),
              guard.position.col
                .until(mapEdges.maxCol)
                .toSet
                .map(col => Coord(col, guard.position.row))
            )
        }

      maybeCollision match
        case Some((newGuard, newLocations)) =>
          // we hit a block, so we'll loop around and go again from the new location
          loop(guard = newGuard, acc = newLocations ++ acc)
        case None =>
          // we're finished, so just add the new locations
          acc ++ finishCells

    loop(startGuard, Set.empty)

  def visualise(
      edges: MapEdges,
      obstacles: Obstacles,
      visited: Set[Coord]
  ): String =
    (0 until edges.maxRow)
      .map { col =>
        val chars = for
          row <- 0 until edges.maxCol
          coord = Coord(col, row)
          isObstacle = obstacles.byColumn
            .get(col)
            .flatMap(_.find(_ == coord))
            .isDefined
          isVisited = visited.contains(coord)
        yield
          if (isVisited && isObstacle) '?'
          else if (isVisited) '!'
          else if (isObstacle) '#'
          else '.'
        chars.mkString
      }
      .mkString("\n")