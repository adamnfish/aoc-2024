package com.adamnfish.days

import com.adamnfish.days.Day07.Equation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFreeSpec with Matchers {
  "Part2.canBeSolved" - {
    "returns true for a simple addition equation" in {
      val equation = Equation(6, Seq(1, 2, 3))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }
    "returns true for another simple addition equation" in {
      val equation = Equation(6, Seq(3, 2, 1))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }
    "returns true for a simple multiplication equation" in {
      val equation = Equation(24, Seq(4, 3, 2))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }
    "returns true for a concatenation example" in {
      val equation = Equation(123, Seq(1, 2, 3))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }
    "example 156: 15 6" in {
      val equation = Equation(156, Seq(15, 6))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }
    "example 7290: 6 8 6 15" in {
      val equation = Equation(7290, Seq(6, 8, 6, 15))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }
    "example 192: 17 8 14" in {
      val equation = Equation(192, Seq(17, 8, 14))
      Day07.Part2.canBeSolved(equation) shouldEqual true
    }

    "false example 161011: 16 10 13" in {
      val equation = Equation(161011, Seq(16, 10, 13))
      Day07.Part2.canBeSolved(equation) shouldEqual false
    }
  }
}
