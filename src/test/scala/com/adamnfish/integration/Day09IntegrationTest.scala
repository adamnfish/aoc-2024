package com.adamnfish.integration

import cats.effect.testing.scalatest.AsyncIOSpec
import com.adamnfish.days.Day06.*
import com.adamnfish.days.Day09
import fs2.Stream
import org.scalatest.freespec.{AnyFreeSpec, AsyncFreeSpec}
import org.scalatest.matchers.should.Matchers

class Day09IntegrationTest
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers {
  "Day09 example" - {
    "day1" in {
      for result <- Day09.part1("example")
      yield result shouldEqual 1928
    }
  }
}
