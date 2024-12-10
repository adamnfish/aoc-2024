package com.adamnfish.days

import cats.effect.testing.scalatest.AsyncIOSpec
import com.adamnfish.days.Day06.*
import fs2.Stream
import org.scalatest.freespec.{AnyFreeSpec, AsyncFreeSpec}
import org.scalatest.matchers.should.Matchers

class Day06Test extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  "getMappedArea" - {
    "for an empty map" - {
      val emptyInput = Stream.emits(
        List(
          "...",
          "...",
          "..."
        )
      )

      "returns correct map edges" in {
        for ((mapEdges, _, _) <- getMappedArea(emptyInput))
          yield mapEdges shouldEqual MapEdges(0, 3, 0, 3)
      }

      "returns no obstacles" in {
        for (_, obstacles, _) <- getMappedArea(emptyInput)
        yield obstacles shouldEqual Obstacles(
          byColumn = Map.empty,
          byRow = Map(
            0 -> Set.empty,
            1 -> Set.empty,
            2 -> Set.empty
          )
        )
      }

      "returns no Guard" in {
        for (_, _, guard) <- getMappedArea(emptyInput)
        yield guard shouldEqual None
      }
    }

    "for a populated map" - {
      val populatedInput = Stream.emits(
        List(
          "...",
          "..#",
          "..."
        )
      )

      "returns the obstacles" in {
        for (_, obstacles, _) <- getMappedArea(populatedInput)
        yield obstacles shouldEqual Obstacles(
          byColumn = Map(
            2 -> Set(Coord(2, 1))
          ),
          byRow = Map(
            0 -> Set.empty,
            1 -> Set(Coord(2, 1)),
            2 -> Set.empty
          )
        )
      }

      "returns no Guard" in {
        for (_, _, guard) <- getMappedArea(populatedInput)
        yield guard shouldEqual None
      }
    }

    "for a more complicated populated map" - {
      val complicatedInput = Stream.emits(
        List(
          "...",
          "..#",
          ".#.",
          "..."
        )
      )

      "returns the obstacles" in {
        for (_, obstacles, _) <- getMappedArea(complicatedInput)
        yield obstacles shouldEqual Obstacles(
          byColumn = Map(
            2 -> Set(Coord(2, 1)),
            1 -> Set(Coord(1, 2))
          ),
          byRow = Map(
            0 -> Set.empty,
            1 -> Set(Coord(2, 1)),
            2 -> Set(Coord(1, 2)),
            3 -> Set.empty
          )
        )
      }

      "returns no Guard" in {
        for (_, _, guard) <- getMappedArea(complicatedInput)
        yield guard shouldEqual None
      }
    }

    "for a map with a guard" - {
      val guardInput = Stream.emits(
        List(
          "...",
          "..^",
          "..."
        )
      )

      "returns the guard" in {
        for (_, _, guard) <- getMappedArea(guardInput)
        yield guard shouldEqual Some(Guard(Direction.Up, Coord(2, 1)))
      }
    }
  }
}
