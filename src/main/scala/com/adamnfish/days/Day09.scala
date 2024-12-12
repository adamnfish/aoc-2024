package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.Parsing.*
import com.adamnfish.{Parsing, Tools}
import fs2.io.file.{Files, Path}
import fs2.{Pipe, text}

import scala.annotation.tailrec

object Day09:
  def part1(inputFile: String) =
    for
      filesystemLayout <- Files[IO]
        .readAll(Path(s"puzzle-inputs/day09/$inputFile.txt"))
        .through(text.utf8.decode)
        .through(text.string2char)
        .takeWhile(_.isDigit)
        .evalMap(c => IO(c.toString.toInt))
        .scan((BlockMode.FileMode, 0, Vector.empty[Block]))(buildDeviceMapping)
        .map(_._3)
        .compile
        .lastOrError
      defragged = defrag(filesystemLayout)
    yield checksum(defragged)

  def part2(inputFile: String) =
    Tools.inputLines("9", inputFile).compile.lastOrError

  def slidingPairs[F[_]: MonadThrow, A]: Pipe[F, A, (A, A)] =
    _.sliding(2, 2)
      .evalMap { pairChunk =>
        pairChunk.toList match
          case first :: second :: Nil =>
            MonadThrow[F].pure((first, second))
          case actual =>
            MonadThrow[F].raiseError(
              new RuntimeException(
                s"Expected two-element chunk to build a pair, got ${actual.mkString(",")}"
              )
            )
      }

  enum BlockMode:
    case FileMode
    case SpaceMode

  enum Block:
    case File(id: Int, size: Int)
    case Space(size: Int)

    def isFile = this match
      case File(_, _) => true
      case _          => false

    def isSpace = !isFile

  def buildDeviceMapping(
      acc: (BlockMode, Int, Vector[Block]),
      blockSize: Int
  ): (BlockMode, Int, Vector[Block]) =
    val (mode, id, blocks) = acc
    mode match
      case BlockMode.FileMode =>
        if (blockSize > 0)
          (BlockMode.SpaceMode, id + 1, blocks :+ Block.File(id, blockSize))
        else
          (BlockMode.SpaceMode, id + 1, blocks)
      case BlockMode.SpaceMode =>
        if (blockSize > 0)
          (BlockMode.FileMode, id, blocks :+ Block.Space(blockSize))
        else
          (BlockMode.FileMode, id, blocks)

  @tailrec
  def defrag(filesystemLayout: Vector[Block]): Vector[Block] =
    filesystemLayout.span(_.isFile) match
      case (prefixFiles, Seq()) =>
        // we have a contiguous filesystem
        prefixFiles
      case (prefixFiles, Block.Space(size) +: Seq()) =>
        // only trailing Space, we're finished
        prefixFiles
      case (prefixFiles, Block.Space(size) +: undefragmented) =>
        // fill our current space with files from the end of the undefragmented filesystem
        val (movedFileBlocks: Vector[Block.File], remainder: Vector[Block]) =
          shuffleBlocks(size, undefragmented)
        defrag(prefixFiles ++ movedFileBlocks ++ remainder)
      case unexpected @ (_, Seq(_, _*)) =>
        throw new RuntimeException(s"?!?!?!?! $unexpected")

  def shuffleBlocks(
      spaceSize: Int,
      undefragmented: Vector[Block]
  ): (Vector[Block.File], Vector[Block]) =
    @tailrec
    def loop(
        remainingSpace: Int,
        movedBlocks: Vector[Block.File],
        rawRemainder: Vector[Block]
    ): (Vector[Block.File], Vector[Block]) =
      val remainder = dropRightWhile(rawRemainder)(_.isSpace)
      if (remainder.isEmpty) (movedBlocks, Vector.empty)
      else
        val lastFile: Block.File = remainder.last match
          case f: Block.File => f
          case Block.Space(size) =>
            throw new RuntimeException(
              s"?!?!?!?!?! We just excluded this case with dropRightWhile"
            )
        if (lastFile.size == remainingSpace)
          (movedBlocks :+ lastFile, remainder.dropRight(1))
        else if (lastFile.size < remainingSpace)
          loop(
            remainingSpace - lastFile.size,
            movedBlocks :+ lastFile,
            remainder.dropRight(1)
          )
        else // lastFile.size > remainingSpace
          val updatedLastFile =
            Block.File(lastFile.id, lastFile.size - remainingSpace)
          val movedBlock: Block.File = Block.File(lastFile.id, remainingSpace)
          (movedBlocks :+ movedBlock, remainder.dropRight(1) :+ updatedLastFile)

    loop(spaceSize, Vector.empty, dropRightWhile(undefragmented)(_.isSpace))

  def checksum(filesystem: Vector[Block]): Long =
    filesystem
      .collect {
        case f @ Block.File(_, size) if size > 0 => f
      } // we assume this is defragmented
      .foldLeft((0L, 0)) { case ((sum, position), f) =>
        // for each block (in f.size), multiply f.id by the new position
        val midpoint = position + (f.size / 2)
        val thisBlockSum =
          if (f.size % 2 == 0)
            (position * f.id) + ((midpoint * f.id) * (f.size - 1))
          else
            (midpoint * f.id) * f.size
        (sum + thisBlockSum, position + f.size)
      }
      ._1

  def dropRightWhile[A](va: Vector[A])(p: A => Boolean): Vector[A] =
    if (va.isEmpty) va
    else if (p(va.last))
      // only do work if required!
      va.reverseIterator.dropWhile(p).toVector.reverse
    else va
