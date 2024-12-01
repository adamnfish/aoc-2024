package com.adamnfish

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import fastparse.*
import org.scalacheck.Gen

class ParsingTest
    extends AnyFreeSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks:
  "integer" - {
    "parses an integer string" in {
      forAll(Gen.choose(0, Int.MaxValue)) { n =>
        val parseResult = parse(n.toString, Parsing.integer(using _))
        parseResult.get.value shouldEqual n
      }
    }

    "does not parse other text" in {
      forAll(Gen.alphaLowerStr) { str =>
        val parseResult = parse(str, Parsing.integer(using _))
        parseResult.isSuccess shouldEqual false
      }
    }
  }

  "long" - {
    "parses a Long string" in {
      forAll(Gen.chooseNum(0L, Long.MaxValue)) { l =>
        val parseResult = parse(l.toString, Parsing.long(using _))
        parseResult.get.value shouldEqual l
      }
    }

    "does not parse other text" in {
      forAll(Gen.alphaLowerStr) { str =>
        val parseResult = parse(str, Parsing.long(using _))
        parseResult.isSuccess shouldEqual false
      }
    }

    "bigInt" - {
      "parses a BigInt string" in {
        forAll(Gen.numStr) { str =>
          whenever(str.nonEmpty) {
            val parseResult = parse(str, Parsing.bigInt(using _))
            parseResult.get.value shouldEqual BigInt(str)
          }
        }
      }

      "does not parse other text" in {
        forAll(Gen.alphaLowerStr) { str =>
          val parseResult = parse(str, Parsing.bigInt(using _))
          parseResult.isSuccess shouldEqual false
        }
      }
    }
  }
