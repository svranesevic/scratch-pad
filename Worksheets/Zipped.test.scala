import munit.*
import Zipped.*

class ZippedSpec extends FunSuite {

  val u: Unit    = ()
  val s: String  = "42"
  val i: Int     = 42
  val b: Boolean = false

  test("zip product type") {
    val si: (String, Int)                = Zipped(s, i)
    val sis: (String, Int, String)       = Zipped(si, s)
    val sisi: (String, Int, String, Int) = Zipped(sis, i)
    assertEquals(sisi, (s, i, s, i))
  }

  test("zip tuple") {
    val si                               = Zipped(s, i)
    val sisi: (String, Int, String, Int) = Zipped(si, si)
    assertEquals(sisi, (s, i, s, i))
  }

  test("zip with unit") {
    val ui: Int = Zipped(u, i)
    val iu: Int = Zipped(i, u)
    assertEquals(ui, i)
    assertEquals(iu, i)
  }

  test("zip either right") {
    val si: Either[String, Int]     = Right(i)
    val sb: Either[String, Boolean] = Right(b)

    val sisb: Either[String, (Int, Boolean)] = Zipped(si, sb)
    assertEquals(sisb, Right((i, b)))
  }

  test("zip either left") {
    val sb: Either[String, BigDecimal]                 = Right(i)
    val ci: Either[Char, Int]                          = Right(i)
    val sbci: Either[String | Char, (BigDecimal, Int)] = Zipped(sb, ci)
    assertEquals(sbci, Right[String | Char, (BigDecimal, Int)]((i, i)))
  }

  test("zip either") {
    val sb: Either[String, BigDecimal]                                 = Right(i)
    val ci: Either[Char, Int]                                          = Right(i)
    val sbci: Either[String | Char, (BigDecimal, Int)]                 = Zipped(sb, ci)
    val sbcisb: Either[String | Char, (BigDecimal, Int, BigDecimal)]   = Zipped(sbci, sb)
    val expected: Either[String | Char, (BigDecimal, Int, BigDecimal)] = Right((i, i, i))
    assertEquals(sbcisb, expected)
  }
}
