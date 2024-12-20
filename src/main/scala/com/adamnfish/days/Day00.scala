package com.adamnfish.days

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.effect.IO
import com.adamnfish.Tools
import com.adamnfish.Parsing
import com.adamnfish.Parsing.*

object Day00:
  def part1(inputFile: String) =
    Tools.inputLines("0", inputFile).compile.lastOrError

  def part2(inputFile: String) =
    Tools.inputLines("0", inputFile).compile.lastOrError

  object Parser:
    import fastparse.*, NoWhitespace.*
