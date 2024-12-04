package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.{Parsing, Tools}

object Day04:
  def part1(inputFile: String) =
    for {
      lines <- Tools
        .inputLines("4", inputFile)
        .filter(_.nonEmpty)
        .compile
        .toVector
      matrix = lines.map(_.toVector)
    } yield Part1.countXmasMatches(matrix)

  def part2(inputFile: String) =
    for {
      lines <- Tools
        .inputLines("4", inputFile)
        .filter(_.nonEmpty)
        .compile
        .toVector
      matrix = lines.map(_.toVector)
    } yield Part2.countCrossMasMatches(matrix)

  object Part1:
    def countXmasMatches(rawMatrix: Vector[Vector[Char]]): Int =
      val rows = rawMatrix.length
      val cols = rawMatrix.head.length // handle error here?
      // pad the matrix with empty chars
      val matrix = rawMatrix.map(_ ++ Vector('.', '.', '.')) ++ Vector.fill(3)(
        Vector.fill(cols + 3)('.')
      )
      val counts = for {
        x <- 0.until(cols)
        y <- 0.until(rows)
        colChars = (
          matrix(y)(x),
          matrix(y + 1)(x),
          matrix(y + 2)(x),
          matrix(y + 3)(x)
        )
        rowChars = (
          matrix(y)(x),
          matrix(y)(x + 1),
          matrix(y)(x + 2),
          matrix(y)(x + 3)
        )
        diagonalChars = (
          matrix(y)(x),
          matrix(y + 1)(x + 1),
          matrix(y + 2)(x + 2),
          matrix(y + 3)(x + 3)
        )
        backwardsDiagonal = (
          matrix(y)(x + 3),
          matrix(y + 1)(x + 2),
          matrix(y + 2)(x + 1),
          matrix(y + 3)(x)
        )
        xmasColCount = charsMatch(colChars)
        xmasRowCount = charsMatch(rowChars)
        xmasDiagonalCount = charsMatch(diagonalChars)
        backwardsDiagonalCount = charsMatch(backwardsDiagonal)
      } yield xmasColCount + xmasRowCount + xmasDiagonalCount + backwardsDiagonalCount
      counts.sum

    def charsMatch(chars: (Char, Char, Char, Char)): Int =
      chars match
        case ('X', 'M', 'A', 'S') => 1
        case ('S', 'A', 'M', 'X') => 1
        case _                    => 0

  object Part2:
    def countCrossMasMatches(rawMatrix: Vector[Vector[Char]]): Int =
      val rows = rawMatrix.length
      val cols = rawMatrix.head.length // handle error here?
      // pad the matrix with empty chars
      val emptyRow = Vector.fill(cols + 2)('.')
      val matrix = emptyRow +: rawMatrix.map('.' +: _ :+ '.') :+ emptyRow
      val counts = for {
        x <- 1.until(cols + 1)
        y <- 2.until(rows + 1)
        tlToBR = (
          matrix(y - 1)(x - 1),
          matrix(y)(x),
          matrix(y + 1)(x + 1)
        )
        trToBl = (
          matrix(y - 1)(x + 1),
          matrix(y)(x),
          matrix(y + 1)(x - 1)
        )
      } yield if (charsMatch(tlToBR) && charsMatch(trToBl)) 1 else 0
      counts.sum

    def charsMatch(chars: (Char, Char, Char)): Boolean =
      chars match
        case ('M', 'A', 'S') => true
        case ('S', 'A', 'M') => true
        case _               => false
