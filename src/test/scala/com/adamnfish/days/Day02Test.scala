package com.adamnfish.days

import com.adamnfish.days.Day02.DirectionChange
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Try, Success}

class Day02Test extends AnyFreeSpec with Matchers {
  "gradual change" - {
    "works for a decreasing sequence" in {
      val input = List(7, 6, 4, 2, 1)
      Day02.gradualChange[Try](input) shouldEqual Success(
        Some(DirectionChange.Decreasing)
      )
    }

    "works for an increasing sequence" in {
      val input = List(1, 3, 6, 7, 9)
      Day02.gradualChange[Try](input) shouldEqual Success(
        Some(DirectionChange.Increasing)
      )
    }

    "fails if the leap is too big (more than 3)" in {
      val input = List(1, 5, 9)
      Day02.gradualChange[Try](input) shouldEqual Success(None)
    }

    "fails if the leap is too big (more than 3) in a decreasing sequence" in {
      val input = List(9, 5, 1)
      Day02.gradualChange[Try](input) shouldEqual Success(None)
    }

    "fails if the sequence changes direction" in {
      val input = List(1, 3, 2, 4)
      Day02.gradualChange[Try](input) shouldEqual Success(None)
    }
  }
}
