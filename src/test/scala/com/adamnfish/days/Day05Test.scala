package com.adamnfish.days

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

class Day05Test extends AnyFreeSpec with Matchers {
  "middleElement" - {
    "returns the middle element of a list" in {
      Day05.middleElement[Try, Int](List(1, 2, 3)) shouldBe Success(2)
    }
    "returns the middle element of a list with an even number of elements" in {
      Day05.middleElement[Try, Int](List(1, 2, 3, 4)).isFailure shouldBe true
    }
    "returns the middle element of a list with an odd number of elements" in {
      Day05.middleElement[Try, Int](List(1, 2, 3, 4, 5)) shouldBe Success(3)
    }
  }
}
