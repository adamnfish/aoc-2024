package com.adamnfish.days

import com.adamnfish.days.Day09.{Block, BlockMode, buildDeviceMapping, defrag}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFreeSpec with Matchers {
  "buildDeviceMapping" - {
    "12345" in {
      val result = List(1, 2, 3, 4, 5).foldLeft(
        (BlockMode.FileMode, 0, Vector.empty[Block])
      )(buildDeviceMapping)
      result._3 shouldEqual Vector(
        Block.File(0, 1),
        Block.Space(2),
        Block.File(1, 3),
        Block.Space(4),
        Block.File(2, 5)
      )
    }
  }

  "defrag" - {
    "works for 0..111....22222 -> 022111222" in {
      val filesystem = Vector(
        Block.File(0, 1),
        Block.Space(2),
        Block.File(1, 3),
        Block.Space(4),
        Block.File(2, 5)
      )
      defrag(filesystem) shouldEqual Vector(
        Block.File(0, 1),
        Block.File(2, 2),
        Block.File(1, 3),
        Block.File(2, 3)
      )
    }
  }

  "checksum" - {
    "0099811188827773336446555566 example" in {
      val filesystem = Vector(
        Block.File(0, 2),
        Block.File(9, 2),
        Block.File(8, 1),
        Block.File(1, 3),
        Block.File(8, 3),
        Block.File(2, 1),
        Block.File(7, 3),
        Block.File(3, 3),
        Block.File(6, 1),
        Block.File(4, 2),
        Block.File(6, 1),
        Block.File(5, 4),
        Block.File(6, 2)
      )
      Day09.checksum(filesystem) shouldEqual 1928L
    }

    "0 example" in {
      val filesystem = Vector(
        Block.File(0, 1)
      )
      Day09.checksum(filesystem) shouldEqual 0L
    }
    "small example" in {
      val filesystem = Vector(
        Block.File(0, 1),
        Block.File(1, 1),
        Block.File(2, 1)
      )
      Day09.checksum(filesystem) shouldEqual 5L
    }
    "different example" in {
      val filesystem = Vector(
        Block.File(0, 1),
        Block.File(1, 2),
        Block.File(2, 3)
      )
      Day09.checksum(filesystem) shouldEqual 27L
    }
  }
}
