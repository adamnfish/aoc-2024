package com.adamnfish

import cats.effect.IO
import fs2.io.file.{Files, Path}
import fs2.{Stream, text}

object Tools:
  def inputLines(day: String, inputFile: String): Stream[IO, String] =
    val paddedDay = day.reverse.padTo(2, '0').reverse
    Files[IO]
      .readAll(Path(s"puzzle-inputs/day$paddedDay/$inputFile.txt"))
      .through(text.utf8.decode)
      .through(text.lines)
