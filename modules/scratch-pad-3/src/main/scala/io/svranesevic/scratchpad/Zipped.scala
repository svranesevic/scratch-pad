package io.svranesevic.scratchpad

import cats.instances.either

type ZipTuple[A, B] =
  B match
    case Tuple => Tuple.Concat[A & Tuple, B & Tuple]
    case _     => Tuple.Append[A & Tuple, B]

def zipTuple[A <: Tuple, B](a: A, b: B): ZipTuple[A, B] =
  b match {
    case b: Tuple => a ++ b
    case _        => a :* b
  }

type ZipAny[A, B] =
  B match
    case Unit  => A
    case Tuple => A *: (B & Tuple)
    case _     => (A, B)

def zipAny[A, B](a: A, b: B): ZipAny[A, B] =
  b match
    case _: Unit  => a
    case b: Tuple => b.*:(a)
    case _        => (a, b)

type ZipEither[L, R, B] =
  B match
    case Either[l, r] => Either[L | l, Zipped[R, r]]
    case _            => Either[L, Zipped[R, B]]

def zipEither[L, R, B](a: Either[L, R], b: B): ZipEither[L, R, B] =
  b match
    case b: Either[l, r] => a.flatMap(a => b.map(b => zipped(a, b)))
    case _               => a.map(a => zipped(a, b))

type Zipped[A, B] =
  A match
    case Unit         => B
    case Tuple        => ZipTuple[A, B]
    case Either[l, r] => ZipEither[l, r, B]
    case _            => ZipAny[A, B]

def zipped[A, B](a: A, b: B): Zipped[A, B] =
  a match {
    case _: Unit         => b
    case a: Tuple        => zipTuple(a, b)
    case a: Either[l, r] => zipEither[l, r, B](a, b)
    case _               => zipAny(a, b)
  }

object Zipped {

  // Scala 3 Match Types: https://dotty.epfl.ch/docs/reference/new-types/match-types.html

  trait Parser[A] { self =>
    def map[B](f: A => B): Parser[B]             = ???
    def flatMap[B](f: A => Parser[B]): Parser[B] = ???

    def zip[B](that: Parser[B]): Parser[Zipped[A, B]] =
      self.flatMap(a => that.map(b => zipped(a, b)))
  }

  val string: Parser[String]                                 = ???
  val int: Parser[Int]                                       = ???
  val unit: Parser[Unit]                                     = ???
  val stringInt: Parser[(String, Int)]                       = string zip int
  val stringIntString: Parser[(String, Int, String)]         = string zip int zip string
  val stringIntStringInt: Parser[(String, Int, String, Int)] = string zip int zip string zip int

  val leftUnit: Parser[Int]  = unit zip int
  val rightUnit: Parser[Int] = int zip unit

  val zipTuples: Parser[(String, Int, String, Int)] = stringInt zip stringInt

  val eitherZipRight: Parser[Either[String, (Int, Boolean)]] = {
    val eitherStringInt: Parser[Either[String, Int]]      = ???
    val eitherStringBool: Parser[Either[String, Boolean]] = ???
    eitherStringInt zip eitherStringBool
  }

  val eitherExtendLeftZipRight: Parser[Either[String | Char, (BigDecimal, Int)]] = {
    val eitherStringBD: Parser[Either[String, BigDecimal]] = ???
    val eitherCharInt: Parser[Either[Char, Int]]           = ???
    eitherStringBD zip eitherCharInt
  }

  val eitherZipRecursive: Parser[Either[String | Char, (BigDecimal, String, Int, Int, Int)]] = {
    val eitherStringBD: Parser[Either[String, BigDecimal]] = ???
    val eitherCharInt: Parser[Either[Char, Int]]           = ???

    eitherStringBD zip stringInt zip int zip eitherCharInt
  }
}
