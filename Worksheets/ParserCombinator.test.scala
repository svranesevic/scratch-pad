import munit.*
import ParserCombinator.Parser as P
import P.*
import P.given

class ParserCombinatorSpec extends FunSuite {

  test("str") {
    var obtained = P.str("void main").parse("void main(int argc)")
    val expected = Right(("void main", "(int argc)"))
    assertEquals(obtained, expected)
  }

  test("char") {
    val obtained = P.char('v').parse("void main(int argc)")
    val expected = Right('v', "oid main(int argc)")
    assertEquals(obtained, expected)
  }

  test("zip - ~") {
    val obtained = (P.str("void") ~ P.str(" main")).parse("void main()")
    val expected = Right(("void", " main"), "()")
    assertEquals(obtained, expected)
  }

  test("adt parse") {
    sealed trait SimpleAdt
    case object Void extends SimpleAdt
    case object Int  extends SimpleAdt

    val orR1 = (("void" ~> Void) | ("int" ~> Int)).parse("int a")
    val orR2 = (("void" ~> Void) | ("int" ~> Int)).parse("void a")
    assertEquals(orR1, Right(Int, " a"))
    assertEquals(orR2, Right(Void, " a"))
  }

  test("mix of zip and or") {
    val A            = P.char('A')
    val B            = P.char('B')
    val C            = P.char('C')
    val bOrElseC     = B | C
    val aAndThenBorC = A ~ bOrElseC
    assertEquals(aAndThenBorC.parse("ABZ"), Right(('A', 'B'), "Z"))
    assertEquals(aAndThenBorC.parse("ACZ"), Right(('A', 'C'), "Z"))
    assertEquals(aAndThenBorC.parse("QBZ"), Left("Expected: A, got QBZ"))
    assertEquals(aAndThenBorC.parse("AQZ"), Left("Expected: C, got QZ"))
  }

  test("type-level zip") {
    // Zipping without tuple nesting!
    val _: P[(Char, Char, Char)]             = 'a' ~ 'b' ~ 'c'
    val _: P[(Char, Char, Char, Char)]       = 'a' ~ 'b' ~ 'c' ~ 'd'
    val _: P[(Char, Char, Char, Char, Char)] = 'a' ~ 'b' ~ 'c' ~ 'd' ~ 'e'

    // Zipping 'penetrates' into Either and avoids tuple nesting!
    val _: P[Either[Int, (Char, Char, Long)]] = {
      val eitherCC: P[Either[Int, (Char, Char)]] = P.const(Right('a' -> 'b'))
      val eitherL: P[Either[Int, Long]]          = P.const(Right(42L))
      eitherCC ~ eitherL
    }
  }

  test("contextual parsing") {
    val digit            = regex("[0-9]").map(_.toInt)
    val contextualParser = digit ~~> (d => str((d * 2).toString))
    val obtained         = contextualParser.parse("510")
    val expected         = Right(("10", ""))
    assertEquals(obtained, expected)
  }

  test("many") {
    val p = "A".many
    assertEquals(p.parse("ABCD"), Right(("A" :: Nil, "BCD")))
    assertEquals(p.parse("AACD"), Right(("A" :: "A" :: Nil, "CD")))
    assertEquals(p.parse("AAAD"), Right(("A" :: "A" :: "A" :: Nil, "D")))
    assertEquals(p.parse("|BCD"), Right((Nil, "|BCD")))
  }

  test("many sep") {
    val p = "A".many(' '.many *> ',' <* ' '.many)
    assertEquals(p.parse("B"), Right((List.empty, "B")))
    assertEquals(p.parse("A"), Right((List("A"), "")))
    assertEquals(p.parse("A,A,A"), Right(List("A", "A", "A"), ""))
    assertEquals(p.parse("A ,A , A"), Right((List("A", "A", "A"), "")))
    assertEquals(p.parse("A, A,   A"), Right(List("A", "A", "A"), ""))
    assertEquals(p.parse("A , A,A"), Right(List("A", "A", "A"), ""))
  }

  test("many1") {
    val p = P.digit.many1
    assertEquals(p.parse("1ABC"), Right((1 :: Nil, "ABC")))
    assertEquals(p.parse("12BC"), Right((1 :: 2 :: Nil, "BC")))
    assertEquals(p.parse("123C"), Right((1 :: 2 :: 3 :: Nil, "C")))
    assertEquals(p.parse("1234"), Right((1 :: 2 :: 3 :: 4 :: Nil, "")))
    assertEquals(p.parse("ABC"), Left("Expected regex: [0-9], got ABC"))
  }

  test("opt") {
    val p = digit ~ char(';').opt
    assertEquals(p.parse("1;"), Right((1, Some(';')), ""))
    assertEquals(p.parse("1"), Right((1, None), ""))
  }

  test("sharks - *> <*") {
    val p = char('(') *> str("hello-world") <* char(')')
    assertEquals(p.parse("(hello-world)"), Right(("hello-world", "")))
    assertEquals(p.parse("(hello-world"), Left("Expected: ), got "))
    assertEquals(p.parse("hello-world)"), Left("Expected: (, got hello-world)"))
    assertEquals(p.parse("hello-world"), Left("Expected: (, got hello-world"))
  }

  test("oneOf") {
    val p = P.oneOf(
      P.str("Forty Six"),
      P.str("Forty Six & 2")
    )
    val obtained = p.parse("Forty Six & 2")
    val expected = Right(("Forty Six & 2", ""))
    assertEquals(obtained, expected)
  }

  test("json") {
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

      import JValue.*

      // Grammar per https://www.json.org/json-en.html, but without escaping and number fraction&exponent

      private def jsonParser = element

      private def value: P[JValue] =
        P.oneOf(`object`, array, string, number, boolean, `null`)

      private def `object`: P[JObject] =
        (('{' *> whitespace <* '}') ~> JObject(Nil)) |
          (('{' *> members <* '}') ~> JObject.apply)

      private def members: P[List[(String, JValue)]] =
        member.many1(P.char(','))

      private def member: P[(String, JValue)] =
        (whitespace *> '"' *> characters <* '"' <* whitespace) ~ (char(':') *> element)

      private def array: P[JArray] =
        (('[' *> whitespace <* ']') ~> JArray(Nil)) |
          (('[' *> elements <* ']') ~> JArray.apply)

      private def elements: P[List[JValue]] = element.many1(P.char(','))

      private def element: P[JValue] = whitespace *> value <* whitespace

      private def string: P[JString] = '"' *> characters.map(JString.apply) <* '"'

      private def characters: P[String] = character.many ~> (_.mkString)

      private def character: P[String] =
        P.oneOf(allowedCharacters).map(_.toString)
      private def allowedCharacters = (' '.to('~')).filter(c => c != '"' && c != '\\').map(char)

      private def number: P[JNumber] =
        integer.map(JNumber.apply)

      private def integer: P[Long] =
        (oneNine.zipWith(digits)(_ +: _)) ~> (_.mkString.toLong) |
          digit ~> (_.toLong) |
          (P.str("-") ~ oneNine ~ digits) ~> { case (a, b, c) => (a +: b +: c).mkString.toLong } |
          (P.str("-").zipWith(digit)(_.concat(_))) ~> (_.toLong)
      private def digits: P[List[String]] = digit.many1
      private def digit: P[String]        = P.str("0") | oneNine
      private def oneNine: P[String]      = P.regex("[1-9]")

      private def boolean: P[JBoolean] = ("false" ~> JBoolean(false)) | ("true" ~> JBoolean(true))
      private def `null`: P[JNull]     = "null" ~> JNull

      private def whitespace: P[?] = P.regex("\\s*")

      def parse(json: String): Either[String, JValue] = jsonParser.parse(json).map(_._1)
    }

    val Right(parsedJson) =
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
      import JsonParser.JValue.*

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
    assertEquals(parsedJson, expectedJson)
  }
}
