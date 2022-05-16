package io.svranesevic.scratchpad

import scala.annotation.tailrec

trait Parser[+A] {
  self =>

  import Parser.Result._

  def run(input: String): Parser.Result[A]

  def ~[B](other: Parser[B]): Parser[(A, B)] =
    for {
      left <- self
      right <- other
    } yield left -> right

  def |[C >: A, B <: C](other: => Parser[B]): Parser[C] =
    (input: String) =>
      run(input) match {
        case Failure(_)        => other.run(input)
        case s @ Success(_, _) => s
      }

  def as[B](b: B): Parser[B] = self.map(_ => b)

  def ~>[B](f: A => B): Parser[B] = map(f)
  def map[B](f: A => B): Parser[B] =
    (input: String) =>
      run(input) match {
        case f @ Failure(_)        => f
        case Success(value, input) => Success(f(value), input)
      }

  def ~~>[B](fp: A => Parser[B]): Parser[B] = flatMap(fp)
  def flatMap[B](fp: A => Parser[B]): Parser[B] =
    (input: String) =>
      run(input) match {
        case f @ Failure(_)        => f
        case Success(value, input) => fp(value).run(input)
      }

  def *>[B](p: Parser[B]): Parser[B] =
    (self ~ p).map(_._2)

  def <*(p: Parser[_]): Parser[A] =
    (self ~ p).map(_._1)
}

object Parser {

  import Parser.Result._

  implicit def strToParser(s: String): Parser[String] = string(s)

  implicit def char(c: Char): Parser[Char] = character(c)

  def succeed[A](a: A): Parser[A] = (input: String) => Success(a, input)

  def sequenceStackUnsafe[A](ps: List[Parser[A]]): Parser[List[A]] =
    (input: String) =>
      ps match {
        case Nil => Success(Nil, input)
        case head :: tail =>
          def cons(headAndTail: (A, List[A])): List[A] = headAndTail._1 +: headAndTail._2

          val parser = head ~ sequence(tail) ~> cons
          parser.run(input)
      }

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    (input: String) => {
      val res = scala.collection.mutable.ListBuffer[A]()

      @tailrec
      def go(input: String, ps: List[Parser[A]]): Result[List[A]] = {
        ps match {
          case Nil => Success(res.toList, input)
          case p :: ps =>
            p.run(input) match {
              case f @ Failure(_) => f
              case Success(value, input) =>
                res += value
                go(input, ps)
            }
        }
      }

      go(input, ps)
    }

  def string(s: String): Parser[String] =
    (input: String) =>
      if (input.startsWith(s)) Success(s, input.drop(s.length))
      else Failure(s"Expected: $s, got $input")

  def character(c: Char): Parser[Char] = {
    val cAsStr = c.toString
    (input: String) =>
      if (input.startsWith(cAsStr)) Success(c, input.drop(1))
      else Failure(s"Expected: $cAsStr, got $input")
  }

  def zeroOrMoreStackUnsafe[A](p: Parser[A]): Parser[List[A]] = oneOrMore(p) | Parser.succeed(Nil)

  def zeroOrMore[A](p: Parser[A]): Parser[List[A]] =
    (input: String) => {
      val res = scala.collection.mutable.ListBuffer[A]()

      @tailrec
      def go(input: String): Result[List[A]] = {
        p.run(input) match {
          case Failure(_) => Success(res.toList, input)
          case Success(value, input) =>
            res += value
            go(input)
        }
      }

      go(input)
    }

  def zeroOrMoreStackUnsafe[A](p: Parser[A], separator: Parser[_]): Parser[List[A]] =
    for {
      head <- p
      tail <- (separator *> zeroOrMoreStackUnsafe(p, separator)) | Parser.succeed(Nil)
    } yield head +: tail

  def zeroOrMore[A](p: Parser[A], separator: Parser[_]): Parser[List[A]] =
    (input: String) => {
      val res = scala.collection.mutable.ListBuffer[A]()

      @tailrec
      def go(input: String): Result[List[A]] = {
        (separator *> p).run(input) match {
          case Failure(_) => Success(res.toList, input)
          case Success(value, input) =>
            res += value
            go(input)
        }
      }

      p.run(input) match {
        case Success(head, input) =>
          res += head
          go(input)
        case Failure(_) => Success(Nil, input)
      }
    }

  def oneOrMore[A](p: Parser[A]): Parser[List[A]] =
    for {
      head <- p
      tail <- zeroOrMore(p)
    } yield head +: tail

  def optional[A](p: Parser[A]): Parser[Option[A]] =
    (input: String) =>
      p.run(input) match {
        case Failure(_)            => Success(None, input)
        case Success(value, input) => Success(Some(value), input)
      }

  sealed trait Result[+A]

  object Result {
    case class Success[A](value: A, input: String) extends Result[A]
    case class Failure(cause: String) extends Result[Nothing]
  }
}

object ParserCombinatorsMain extends App {

  import Parser.Result._
  import Parser._

  // String
  val stringPResult = string("void main").run("void main(int argc)")
  assert(stringPResult == Success("void main", "(int argc)"))

  // Char
  val charPResult = character('v').run("void main(int argc)")
  assert(charPResult == Success('v', "oid main(int argc)"))

  // ~
  val andThenResult = (string("void") ~ string(" main")).run("void main()")
  assert(andThenResult == Success(("void", " main"), "()"))

  // ADT + Parse result type widening + DSL syntax with implicit conversions - instead of string("void") & string("int")
  sealed trait SimpleAdt
  case object Void extends SimpleAdt
  case object Int extends SimpleAdt

  val orResult1: Result[SimpleAdt] = (("void" as Void) | ("int" as Int)).run("int a")
  val orResult2: Result[SimpleAdt] = (("void" as Void) | ("int" as Int)).run("void a")
  assert(orResult1 == Success[SimpleAdt](Int, " a"))
  assert(orResult2 == Success[SimpleAdt](Void, " a"))

  // Combine ~ and |
  val A = char('A')
  val B = char('B')
  val C = char('C')
  val bOrElseC = B | C
  val aAndThenBorC = A ~ bOrElseC
  assert(aAndThenBorC.run("ABZ") == Success(('A', 'B'), "Z"))
  assert(aAndThenBorC.run("ACZ") == Success(('A', 'C'), "Z"))
  assert(aAndThenBorC.run("QBZ") == Failure("Expected: A, got QBZ"))
  assert(aAndThenBorC.run("AQZ") == Failure("Expected: C, got QZ"))

  // Tuple issue
  val abc: Parser[((Char, Char), Char)] = 'a' ~ 'b' ~ 'c'

  // Sequencing
  val sequenced: Parser[List[Char]] =
    sequence(character('a') :: character('b') :: character('c') :: Nil)
  val sequencedResult = sequenced.run("abcDGF")
  assert(sequencedResult == Success('a' :: 'b' :: 'c' :: Nil, "DGF"))

  val stacksafeParser = sequence(("a" * 10000).map(character _).toList)
  assert(stacksafeParser.run("a" * 20000) == Success(List.fill(10000)('a'), "a" * 10000))

  // Contextual parsing
  val digit: Parser[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(d => string(d.toString)).reduce(_ | _) ~> (_.toInt)
  val contextualParser: Parser[String] = digit ~~> (d => string((d * 2).toString))
  val contextualParserResult = contextualParser.run("510")
  assert(contextualParserResult == Success("10", ""))

  // Zero or more
  val zeroOrMoreParser: Parser[List[String]] = zeroOrMore("A")
  assert(zeroOrMoreParser.run("ABCD") == Success("A" :: Nil, "BCD"))
  assert(zeroOrMoreParser.run("AACD") == Success("A" :: "A" :: Nil, "CD"))
  assert(zeroOrMoreParser.run("AAAD") == Success("A" :: "A" :: "A" :: Nil, "D"))
  assert(zeroOrMoreParser.run("|BCD") == Success(Nil, "|BCD"))

  // One or more
  val oneOrMoreParser: Parser[List[Int]] = oneOrMore(digit)
  assert(oneOrMoreParser.run("1ABC") == Success(1 :: Nil, "ABC"))
  assert(oneOrMoreParser.run("12BC") == Success(1 :: 2 :: Nil, "BC"))
  assert(oneOrMoreParser.run("123C") == Success(1 :: 2 :: 3 :: Nil, "C"))
  assert(oneOrMoreParser.run("1234") == Success(1 :: 2 :: 3 :: 4 :: Nil, ""))
  assert(oneOrMoreParser.run("ABC") == Failure("Expected: 9, got ABC"))

  // Optional
  val digitThenSemicolon = digit ~ optional(character(';'))
  assert(digitThenSemicolon.run("1;") == Success((1, Some(';')), ""))
  assert(digitThenSemicolon.run("1") == Success((1, None), ""))

  // Discarding ala Surround-by
  val surroundByParens: Parser[String] = '(' *> string("hello-world") <* ')'
  assert(surroundByParens.run("(hello-world)") == Success("hello-world", ""))
  assert(surroundByParens.run("(hello-world") == Failure("Expected: ), got "))
  assert(surroundByParens.run("hello-world)") == Failure("Expected: (, got hello-world)"))
  assert(surroundByParens.run("hello-world") == Failure("Expected: (, got hello-world"))

  // Zero or more with separator
  val zeroOrMoreSeparatedParser = zeroOrMore("A", zeroOrMore(' ') *> ',' <* zeroOrMore(' '))
  assert(zeroOrMoreSeparatedParser.run("A") == Success(List("A"), ""))
  assert(zeroOrMoreSeparatedParser.run("A,A,A") == Success(List("A", "A", "A"), ""))
  assert(zeroOrMoreSeparatedParser.run("A ,A , A") == Success(List("A", "A", "A"), ""))
  assert(zeroOrMoreSeparatedParser.run("A, A,   A") == Success(List("A", "A", "A"), ""))
  assert(zeroOrMoreSeparatedParser.run("A , A,A") == Success(List("A", "A", "A"), ""))

  // JSON
  object JsonParser {
    sealed trait JValue
    object JValue {
      // Simplified JSON Ast
      case object JNull extends JValue
      case class JString(s: String) extends JValue
      case class JBoolean(value: Boolean) extends JValue
      case class JLong(num: Long) extends JValue
      case class JObject(obj: Map[String, JValue]) extends JValue
      case class JArray(arr: List[JValue]) extends JValue
    }

    import JValue._

    private val letter: Parser[Char] = (('a' to 'z').toList ++ ('A' to 'Z').toList).map(char).reduce(_ | _)
    private val digits: Parser[List[Int]] = oneOrMore(digit)
    private val long: Parser[Long] = digits ~> (_.mkString.toLong)

    private def whitespace: Parser[_] = zeroOrMore(character(' ') | character('\n') | character('\r') | character('\t'))
    private def comma: Parser[_] = whitespace *> ',' <* whitespace

    private def jNull: Parser[JNull.type] = string("null") as JNull

    private def jString: Parser[JString] =
      ('"' *> zeroOrMore(letter | digit | ' ') <* '"') ~> (s => JString(s.mkString))

    private def jBool: Parser[JBoolean] =
      ("false" as JBoolean(false)) | ("true" as JBoolean(true))

    private def jLong: Parser[JLong] = long ~> JLong

    private def jKey: Parser[String] =
      whitespace *> '"' *> oneOrMore(letter | digit) ~> (_.mkString) <* '"' <* whitespace

    private def jValue: Parser[JValue] =
      whitespace *> (jNull | jString | jBool | jLong | jArray | jObject) <* whitespace

    private def jKeyValue: Parser[(String, JValue)] = jKey ~ (":" *> jValue)

    private def jObject: Parser[JObject] =
      ('{' *> whitespace *> zeroOrMore(jKeyValue, comma) <* whitespace <* '}') ~>
        (keyValuePairs => JObject(keyValuePairs.toMap))

    private def jArray: Parser[JArray] =
      ('[' *> whitespace *> zeroOrMore(jValue, comma) <* whitespace <* ']') ~> JArray

    def run(json: String): Result[JValue] = (whitespace *> jObject <* whitespace).run(json)
  }

  val Success(parsedJson, _) =
    JsonParser.run("""
      {
        "id": "15248544",
        "idType": "Snowflake",
        "availableIdTypes": [{ "type": "uuid", "version": 1 }, { "type": "uuid", "version": 4 }, "snowflake"],
        "universe": 42,
        "42": true,
        "41": false,
        "emptyArray": [    ],
        "emptyObject": {}
      }
    """)
  val expectedJson = {
    import JsonParser.JValue._

    JObject(
      Map(
        "id" -> JString("15248544"),
        "idType" -> JString("Snowflake"),
        "availableIdTypes" -> JArray(
          List(
            JObject(
              Map(
                "type" -> JString("uuid"),
                "version" -> JLong(1)
              )
            ),
            JObject(
              Map(
                "type" -> JString("uuid"),
                "version" -> JLong(4)
              )
            ),
            JString("snowflake")
          )
        ),
        "universe" -> JLong(42),
        "42" -> JBoolean(true),
        "41" -> JBoolean(false),
        "emptyArray" -> JArray(List.empty),
        "emptyObject" -> JObject(Map.empty)
      )
    )
  }
  assert(parsedJson == expectedJson)
}
