package io.svranesevic.scratchpad

import scala.annotation.tailrec

trait Parser[+A] {
  self =>

  import Parser.Result._

  def parse(input: String): Parser.Result[A]

  def ~[B](other: Parser[B])(implicit zippable: Zippable[A, B]): Parser[zippable.Out] = zip(other)
  def zip[B](other: Parser[B])(implicit zippable: Zippable[A, B]): Parser[zippable.Out] =
    for {
      left  <- self
      right <- other
    } yield zippable.zip(left, right)

  def zipWith[B, C](other: Parser[B])(fn: (A, B) => C): Parser[C] =
    (self ~ other).map(fn.tupled)

  def |[C >: A, B <: C](other: => Parser[B]): Parser[C] = input =>
    parse(input) match {
      case Failure(_)        => other.parse(input)
      case s @ Success(_, _) => s
    }

  def as[B](b: B): Parser[B] = self.map(_ => b)

  def ~>[B](f: A => B): Parser[B] = map(f)
  def map[B](f: A => B): Parser[B] = input =>
    parse(input) match {
      case f: Failure            => f
      case Success(value, input) => Success(f(value), input)
    }

  def ~~>[B](fp: A => Parser[B]): Parser[B] = flatMap(fp)
  def flatMap[B](fp: A => Parser[B]): Parser[B] = input =>
    parse(input) match {
      case f @ Failure(_)        => f
      case Success(value, input) => fp(value).parse(input)
    }

  def *>[B](p: Parser[B]): Parser[B] =
    self.zipWith(p) { case (_, b) => b }

  def <*(p: Parser[?]): Parser[A] =
    self.zipWith(p) { case (a, _) => a }

  def many: Parser[List[A]]                 = Parser.many(self)
  def many(sep: Parser[?]): Parser[List[A]] = Parser.many(self, sep)

  def many1: Parser[List[A]]                 = Parser.many1(self)
  def many1(sep: Parser[?]): Parser[List[A]] = Parser.many1(self, sep)

  def opt: Parser[Option[A]] = Parser.opt(self)
}

object Parser {

  import Parser.Result._

  implicit def strToParser(s: String): Parser[String] = string(s)

  implicit def char(c: Char): Parser[Char] = character(c)

  def apply[A](parse: String => Result[A]): Parser[A] = parse.apply

  def const[A](a: A): Parser[A] = (input: String) => Success(a, input)

  def sequenceStackUnsafe[A](ps: List[Parser[A]]): Parser[List[A]] = input =>
    ps match {
      case Nil => Success(Nil, input)
      case head :: tail =>
        def cons(headAndTail: (A, List[A])): List[A] = headAndTail._1 +: headAndTail._2

        val parser = head ~ sequenceStackUnsafe(tail) ~> cons
        parser.parse(input)
    }

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] = { input =>
    val res = scala.collection.mutable.ListBuffer[A]()

    @tailrec
    def go(input: String, ps: List[Parser[A]]): Result[List[A]] =
      ps match {
        case Nil => Success(res.toList, input)
        case p :: ps =>
          p.parse(input) match {
            case f @ Failure(_) => f
            case Success(value, input) =>
              res += value
              go(input, ps)
          }
      }

    go(input, ps)
  }

  def regex(pattern: String): Parser[String] = {
    val regex = new scala.util.matching.Regex(s"^($pattern).*")
    Parser { input =>
      regex.findFirstMatchIn(input) match {
        case None =>
          Result.Failure(s"Expected regex: $pattern, got $input")
        case Some(m) =>
          val matchh = m.group(1)
          Result.Success(matchh, input.drop(matchh.size))
      }
    }
  }

  def string(s: String): Parser[String] = input =>
    if (input.startsWith(s)) Success(s, input.drop(s.length))
    else Failure(s"Expected: $s, got $input")

  def character(c: Char): Parser[Char] = {
    val cAsStr = c.toString
    Parser { input =>
      if (input.startsWith(cAsStr)) Success(c, input.drop(1))
      else Failure(s"Expected: $cAsStr, got $input")
    }
  }

  def manyStackUnsafe[A](p: => Parser[A]): Parser[List[A]] = p.many1 | Parser.const(Nil)

  def many[A](p: Parser[A]): Parser[List[A]] = { input =>
    val res = scala.collection.mutable.ListBuffer[A]()

    @tailrec
    def go(input: String): Result[List[A]] =
      p.parse(input) match {
        case Failure(_) => Success(res.toList, input)
        case Success(value, input) =>
          res += value
          go(input)
      }

    go(input)
  }

  def manyStackUnsafe[A](p: => Parser[A], separator: => Parser[?]): Parser[List[A]] =
    for {
      head <- p
      tail <- (separator *> manyStackUnsafe(p, separator)) | Parser.const(Nil)
    } yield head +: tail

  def many[A](p: Parser[A], separator: Parser[?]): Parser[List[A]] =
    (input: String) => {
      val res = scala.collection.mutable.ListBuffer[A]()

      @tailrec
      def go(input: String): Result[List[A]] =
        (separator *> p).parse(input) match {
          case Failure(_) => Success(res.toList, input)
          case Success(value, input) =>
            res += value
            go(input)
        }

      p.parse(input) match {
        case Success(head, input) =>
          res += head
          go(input)
        case Failure(_) => Success(Nil, input)
      }
    }

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      head <- p
      tail <- many(p)
    } yield head +: tail

  def many1[A](p: Parser[A], separator: Parser[?]): Parser[List[A]] =
    (input: String) => {
      val res = scala.collection.mutable.ListBuffer[A]()

      @tailrec
      def go(input: String): Result[List[A]] =
        (separator *> p).parse(input) match {
          case Failure(_) => Success(res.toList, input)
          case Success(value, input) =>
            res += value
            go(input)
        }

      p.parse(input) match {
        case Success(head, input) =>
          res += head
          go(input)
        case f @ Failure(_) => f
      }
    }

  def opt[A](p: Parser[A]): Parser[Option[A]] = { input =>
    p.parse(input) match {
      case Failure(_)            => Success(None, input)
      case Success(value, input) => Success(Some(value), input)
    }
  }

  def oneOf[A](p: Parser[A], ps: Parser[A]*): Parser[A] =
    oneOf(p +: ps)

  def oneOf[A](ps: Iterable[Parser[A]]): Parser[A] = { input =>
    val results = ps.toVector.map(_.parse(input))

    val success = results
      .collect { case s: Result.Success[A] => s }
      .sortBy(_.input.size)

    success.headOption
      .orElse(results.headOption)
      .getOrElse(throw new IllegalStateException("oneOf: Must have at least one Parser supplied"))
  }

  sealed trait Result[+A] {
    def isSuccess: Boolean = this match {
      case _: Success[?] => true
      case _             => false
    }
  }

  object Result {
    case class Success[A](value: A, input: String) extends Result[A]
    case class Failure(cause: String)              extends Result[Nothing]
  }
}

object ParserCombinatorsMain extends App {

  import Parser.Result._
  import Parser._

  // String
  val stringPResult = string("void main").parse("void main(int argc)")
  assert(stringPResult == Success("void main", "(int argc)"))

  // Char
  val charPResult = character('v').parse("void main(int argc)")
  assert(charPResult == Success('v', "oid main(int argc)"))

  // ~
  val andThenResult = (string("void") ~ string(" main")).parse("void main()")
  assert(andThenResult == Success(("void", " main"), "()"))

  // ADT + Parse result type widening + DSL syntax with implicit conversions - instead of string("void") & string("int")
  sealed trait SimpleAdt
  case object Void extends SimpleAdt
  case object Int  extends SimpleAdt

  val orResult1: Result[SimpleAdt] = (("void" as Void) | ("int" as Int)).parse("int a")
  val orResult2: Result[SimpleAdt] = (("void" as Void) | ("int" as Int)).parse("void a")
  assert(orResult1 == Success[SimpleAdt](Int, " a"))
  assert(orResult2 == Success[SimpleAdt](Void, " a"))

  // Combine ~ and |
  val A            = char('A')
  val B            = char('B')
  val C            = char('C')
  val bOrElseC     = B | C
  val aAndThenBorC = A ~ bOrElseC
  assert(aAndThenBorC.parse("ABZ") == Success(('A', 'B'), "Z"))
  assert(aAndThenBorC.parse("ACZ") == Success(('A', 'C'), "Z"))
  assert(aAndThenBorC.parse("QBZ") == Failure("Expected: A, got QBZ"))
  assert(aAndThenBorC.parse("AQZ") == Failure("Expected: C, got QZ"))

  // Zipping without tuple nesting!
  val abc: Parser[(Char, Char, Char)]               = 'a' ~ 'b' ~ 'c'
  val abcd: Parser[(Char, Char, Char, Char)]        = 'a' ~ 'b' ~ 'c' ~ 'd'
  val abcde: Parser[(Char, Char, Char, Char, Char)] = 'a' ~ 'b' ~ 'c' ~ 'd' ~ 'e'

  // Zipping 'penetrates' into Either and avoids tuple nesting!
  val zippedEither: Parser[Either[Int, (Char, Char, Long)]] = {
    val eitherCC: Parser[Either[Int, (Char, Char)]] = Parser.const(Right('a' -> 'b'))
    val eitherL: Parser[Either[Int, Long]]          = Parser.const(Right(42L))
    eitherCC zip eitherL
  }

  // Sequencing
  val sequenced: Parser[List[Char]] =
    sequence(character('a') :: character('b') :: character('c') :: Nil)
  val sequencedResult = sequenced.parse("abcDGF")
  assert(sequencedResult == Success('a' :: 'b' :: 'c' :: Nil, "DGF"))

  val stacksafeParser = sequence(("a" * 10000).toCharArray().map(character _).toList)
  assert(stacksafeParser.parse("a" * 20000) == Success(List.fill(10000)('a'), "a" * 10000))

  // Contextual parsing
  val digit: Parser[Int]               = Parser.regex("[0-9]") ~> (_.toInt)
  val contextualParser: Parser[String] = digit ~~> (d => string((d * 2).toString))
  val contextualParserResult           = contextualParser.parse("510")
  assert(contextualParserResult == Success("10", ""))

  // Zero or more
  val manyParser: Parser[List[String]] = "A".many
  assert(manyParser.parse("ABCD") == Success("A" :: Nil, "BCD"))
  assert(manyParser.parse("AACD") == Success("A" :: "A" :: Nil, "CD"))
  assert(manyParser.parse("AAAD") == Success("A" :: "A" :: "A" :: Nil, "D"))
  assert(manyParser.parse("|BCD") == Success(Nil, "|BCD"))

  // One or more
  val many1Parser: Parser[List[Int]] = digit.many1
  assert(many1Parser.parse("1ABC") == Success(1 :: Nil, "ABC"))
  assert(many1Parser.parse("12BC") == Success(1 :: 2 :: Nil, "BC"))
  assert(many1Parser.parse("123C") == Success(1 :: 2 :: 3 :: Nil, "C"))
  assert(many1Parser.parse("1234") == Success(1 :: 2 :: 3 :: 4 :: Nil, ""))
  assert(many1Parser.parse("ABC") == Failure("Expected regex: [0-9], got ABC"))

  // Optional
  val digitThenSemicolon = digit ~ character(';').opt
  assert(digitThenSemicolon.parse("1;") == Success((1, Some(';')), ""))
  assert(digitThenSemicolon.parse("1") == Success((1, None), ""))

  // Discarding ala Surround-by
  val surroundByParens: Parser[String] = '(' *> string("hello-world") <* ')'
  assert(surroundByParens.parse("(hello-world)") == Success("hello-world", ""))
  assert(surroundByParens.parse("(hello-world") == Failure("Expected: ), got "))
  assert(surroundByParens.parse("hello-world)") == Failure("Expected: (, got hello-world)"))
  assert(surroundByParens.parse("hello-world") == Failure("Expected: (, got hello-world"))

  // Zero or more with separator
  val manySeparatedParser = "A".many(' '.many *> ',' <* ' '.many)
  assert(manySeparatedParser.parse("A") == Success(List("A"), ""))
  assert(manySeparatedParser.parse("A,A,A") == Success(List("A", "A", "A"), ""))
  assert(manySeparatedParser.parse("A ,A , A") == Success(List("A", "A", "A"), ""))
  assert(manySeparatedParser.parse("A, A,   A") == Success(List("A", "A", "A"), ""))
  assert(manySeparatedParser.parse("A , A,A") == Success(List("A", "A", "A"), ""))

  // Greedy oneOf
  val greedyOneOf = Parser.oneOf(
    Parser.string("Forty Six"),
    Parser.string("Forty Six & 2")
  )
  assert(greedyOneOf.parse("Forty Six & 2") == Success("Forty Six & 2", ""))

  // JSON
  object JsonParser {
    sealed trait JValue
    object JValue {
      case class JObject(obj: List[(String, JValue)]) extends JValue
      case class JArray(arr: List[JValue])            extends JValue
      case class JString(s: String)                   extends JValue
      case class JNumber(num: Long)                   extends JValue
      case class JBoolean(value: Boolean)             extends JValue
      case object JNull                               extends JValue
      type JNull = JNull.type
    }

    import JValue._

    // Grammar per https://www.json.org/json-en.html, but without escaping and number fraction&exponent

    private def jsonParser = element

    private def value: Parser[JValue] =
      `object` | array | string | number | boolean | `null`

    private def `object`: Parser[JObject] =
      (('{' *> whitespace <* '}') as JObject(Nil)) |
        (('{' *> members <* '}') ~> JObject.apply)

    private def members: Parser[List[(String, JValue)]] =
      member.many1(Parser.char(','))

    private def member: Parser[(String, JValue)] =
      (whitespace *> '"' *> characters <* '"' <* whitespace) ~ (char(':') *> element)

    private def array: Parser[JArray] =
      (('[' *> whitespace <* ']') as JArray(Nil)) |
        (('[' *> elements <* ']') ~> JArray.apply)

    private def elements: Parser[List[JValue]] = element.many1(Parser.char(','))

    private def element: Parser[JValue] = whitespace *> value <* whitespace

    private def string: Parser[JString] = '"' *> characters ~> JString.apply <* '"'

    private def characters: Parser[String] = character.many ~> (_.mkString)

    private def character: Parser[String] =
      Parser.oneOf(allowedCharacters).map(_.toString)
    private def allowedCharacters = (' ' to '~').filter(c => c != '"' && c != '\\').map(char)

    private def number: Parser[JNumber] =
      integer ~> JNumber.apply

    private def integer: Parser[Long] =
      (oneNine.zipWith(digits)(_ +: _)) ~> (_.mkString.toLong) |
        digit ~> (_.toLong) |
        (Parser.string("-") ~ oneNine ~ digits) ~> { case (a, b, c) => (a +: b +: c).mkString.toLong } |
        (Parser.string("-").zipWith(digit)(_ concat _)) ~> (_.toLong)
    private def digits: Parser[List[String]] = digit.many1
    private def digit: Parser[String]        = Parser.string("0") | oneNine
    private def oneNine: Parser[String]      = Parser.regex("[1-9]")

    private def boolean: Parser[JBoolean] = ("false" as JBoolean(false)) | ("true" as JBoolean(true))
    private def `null`: Parser[JNull]     = "null" as JNull

    private def whitespace: Parser[?] = Parser.regex("\\s*")

    def parse(json: String): Result[JValue] = jsonParser.parse(json)
  }

  val Success(parsedJson, _) =
    JsonParser.parse("""
      {
        "id": "15248544",
        "idType": "Snowflake",
        "availableIdTypes": [{ "type": "uuid", "version": 1 }, { "type": "uuid", "version": 4 }, "snowflake"],
        "universe": 42,
        "oneOverUniverse": -42,
        "42": true,
        "41": false,
        "emptyArray": [    ],
        "emptyObject": {}
      }
    """): @unchecked
  val expectedJson = {
    import JsonParser.JValue._

    JObject(
      List(
        "id"     -> JString("15248544"),
        "idType" -> JString("Snowflake"),
        "availableIdTypes" ->
          JArray(
            List(
              JObject(
                List(
                  "type"    -> JString("uuid"),
                  "version" -> JNumber(1)
                )
              ),
              JObject(
                List(
                  "type"    -> JString("uuid"),
                  "version" -> JNumber(4)
                )
              ),
              JString("snowflake")
            )
          ),
        "universe"        -> JNumber(42),
        "oneOverUniverse" -> JNumber(-42),
        "42"              -> JBoolean(true),
        "41"              -> JBoolean(false),
        "emptyArray"      -> JArray(Nil),
        "emptyObject"     -> JObject(Nil)
      )
    )
  }
  assert(parsedJson == expectedJson)
}
