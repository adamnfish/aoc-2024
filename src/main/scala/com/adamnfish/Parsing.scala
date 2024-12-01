package com.adamnfish

import cats.*
import cats.data.*
import cats.syntax.all.*
import cats.MonadThrow
import fastparse.*, NoWhitespace.*

object Parsing:
  extension [A](result: Parsed[A])
    def intoF[F[_] : MonadThrow] =
      result match
        case Parsed.Success(a, index) =>
          MonadThrow[F].pure(a)
        case failure: Parsed.Failure =>
          MonadThrow[F].raiseError(
            RuntimeException(s"Parse error: ${failure.msg}")
          )

  def integer[$: P]: P[Int] =
    P(CharIn("0-9").rep(1).!.map(_.toInt))

  def paddedInteger[$: P]: P[Int] =
    P(" ".rep(min = 0) ~ integer)
  
  def long[$: P]: P[Long] =
    P(CharIn("0-9").rep(1).!.map(_.toLong))
  
  def paddedLong[$: P]: P[Long] =
    P(" ".rep(min = 0) ~ long)

  def bigInt[$: P]: P[BigInt] =
    P(CharIn("0-9").rep(1).!.map(s => BigInt(s)))

  def paddedBigInt[$: P]: P[BigInt] =
    P(" ".rep(min = 0) ~ bigInt)
