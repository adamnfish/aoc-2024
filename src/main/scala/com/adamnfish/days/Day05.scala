package com.adamnfish.days

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.all.*
import com.adamnfish.{Parsing, Tools}
import com.adamnfish.Parsing.*

import javax.naming.ldap.PagedResultsControl
import scala.annotation.tailrec

object Day05:
  def part1(inputFile: String) =
    val lines = Tools.inputLines("5", inputFile)
    for
      pageDependencies <- lines
        .takeWhile(_.nonEmpty, false)
        .evalMap(Parser.parseOrderingRule[IO])
        // store the dependencies for each page
        .scan(Map.empty[Int, Set[Int]]) { case (acc, or) =>
          acc.updatedWith(or.page) {
            case None =>
              Some(Set(or.dependsOn))
            case Some(dependencies) =>
              Some(dependencies + or.dependsOn)
          }
        }
        .compile
        .lastOrError
      middleSum <- lines
        .dropThrough(_.nonEmpty)
        .filter(_.nonEmpty)
        .evalMap(Parser.parsePages[IO])
        // collect info about which pages follow each page, to check against the page dependencies
        .map { pages =>
          (pages, pagesWithFolllowingPages(pages.pages))
        }
        .mapFilter { case (pages, following) =>
          if (isValidPages(following, pageDependencies)) Some(pages)
          else None
        }
        .evalMap(pages => middleElement[IO, Int](pages.pages))
        .scan(0)(_ + _)
        .compile
        .lastOrError
    yield middleSum

  def part2(inputFile: String) =
    Tools.inputLines("5", inputFile).compile.lastOrError

  case class OrderingRule(page: Int, dependsOn: Int)
  case class Pages(pages: List[Int])

  def pagesWithFolllowingPages(pages: List[Int]): List[(Int, Set[Int])] =
    @tailrec
    def loop(
        acc: List[(Int, Set[Int])],
        remainder: List[Int]
    ): List[(Int, Set[Int])] =
      remainder match
        case Nil => acc
        case head :: tail =>
          loop((head, tail.toSet) :: acc, tail)
    loop(Nil, pages)

  def isValidPages(
      followingPages: List[(Int, Set[Int])],
      pageDependencies: Map[Int, Set[Int]]
  ): Boolean =
    followingPages.forall { case (page, following) =>
      val dependencies = pageDependencies.getOrElse(page, Set.empty)
      // if a dependency *follow* this page then this isn't valid
      following.intersect(dependencies).isEmpty
    }

  def middleElement[F[_]: MonadThrow, A](aa: List[A]): F[A] =
    // if even length, raise error
    if aa.length % 2 == 0 then
      MonadThrow[F].raiseError(
        new Exception(
          s"List must have an odd number of elements: ${aa.mkString(",")}"
        )
      )
    else
      val half = aa.length / 2
      MonadThrow[F].pure(aa(half))

  object Parser:
    import fastparse.*
    import NoWhitespace.*

    def parseOrderingRule[F[_]: MonadThrow](input: String): F[OrderingRule] =
      parse(input, parseOrderingRule(using _)).intoF

    private def parseOrderingRule[$: P]: P[OrderingRule] =
      P(integer ~ "|" ~ integer).map { case (dependsOn, page) =>
        OrderingRule(page, dependsOn)
      }

    def parsePages[F[_]: MonadThrow](input: String): F[Pages] =
      parse(input, parsePages(using _)).intoF

    private def parsePages[$: P]: P[Pages] =
      P(integer.rep(sep = ",")).map(ps => Pages(ps.toList))
