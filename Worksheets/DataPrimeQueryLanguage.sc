//> using scala 3.3.1
//> using dep org.parboiled::parboiled:2.5.1
//> using dep com.lihaoyi::pprint:0.8.1
//> using dep org.scalameta::munit:0.7.29

// Parsing interesting bits of Coralogix DataPrime Query Language into an AST
// See https://coralogix.com/docs/dataprime-query-language/

import org.parboiled2.*
import pprint.*

class QueryParser(val input: ParserInput) extends Parser {

  def Parser: Rule1[Query] = rule {
    QueryStatement ~ EOI
  }

  def QueryStatement: Rule1[Query] = rule {
    WS ~ Source ~ zeroOrMore(Pipe ~ Operator) ~> Query.apply
  }

  def WS: Rule0 = rule {
    quiet(zeroOrMore(anyOf(" \n\r\t\f")))
  }

  def WS(char: Char): Rule0 = rule {
    ch(char) ~ WS
  }

  def WS(s: String): Rule0 = rule {
    str(s) ~ WS
  }

  def Keyword(s: String): Rule0 = rule {
    ignoreCase(s.toLowerCase) ~ WS
  }

  def Keywords(s: String*): Rule0 = rule {
    s.map(Keyword).reduce((l, r) => rule(l | r))
  }

  // Grammar

  def Source: Rule1[Query.Source] = rule {
    Keyword("source") ~ capture(oneOrMore(CharPredicate.AlphaNum ++ "_-.")) ~ WS ~> Query.Source.fromString
  }

  def Pipe: Rule0 = rule {
    WS ~ "|" ~ WS
  }

  def KeyPath: Rule1[String] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum)) ~ capture(zeroOrMore(CharPredicate.AlphaNum | '.')) ~ WS ~> (_ + _)
  }

  def Field: Rule1[Query.Field] = rule {
    "$" ~ capture(CharPredicate.Alpha) ~ "." ~ KeyPath ~> Query.Field.apply
  }

  def StringLiteral: Rule1[Query.Literal.StringValue] = rule {
    "'" ~ capture(zeroOrMore(noneOf("'"))) ~ "'" ~ WS ~> Query.Literal.StringValue.apply
  }

  def NumberLiteral: Rule1[Query.Literal.NumberValue] = {
    import CharPredicate.{ Digit, Digit19 }
    def Digits   = rule { oneOrMore(Digit) }
    def Integer  = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }
    def Fraction = rule { "." ~ Digits }
    def Exponent = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }

    rule {
      capture(Integer ~ optional(Fraction) ~ optional(Exponent)) ~ WS ~>
        (str => Query.Literal.NumberValue(BigDecimal(str)))
    }
  }

  def RegExpLiteral: Rule1[Query.Literal.RegExp] = rule {
    "/" ~ capture(zeroOrMore(noneOf("/"))) ~ "/" ~ WS ~> Query.Literal.RegExp.apply
  }

  def KeyPathLiteral: Rule1[Query.Literal.KeyPath] = rule {
    Field ~> Query.Literal.KeyPath.apply
  }

  def Expression: Rule1[Query.Expression] = rule {
    MathExpression |
      StringLiteral |
      NumberLiteral |
      RegExpLiteral |
      KeyPathLiteral
  }

  def ConditionExpression: Rule1[Query.ConditionExpression] = rule {
    oneOrMore(ConditionTerms).separatedBy(WS("||")) ~> {
      case Seq(expr) => expr
      case exprs     => Query.ConditionExpression.Or(exprs)
    }
  }

  def ConditionTerms: Rule1[Query.ConditionExpression] = rule {
    oneOrMore(ConditionFactors).separatedBy(WS("&&")) ~> {
      case Seq(expr) => expr
      case exprs     => Query.ConditionExpression.And(exprs)
    }
  }

  def ConditionFactors: Rule1[Query.ConditionExpression] = rule {
    WS("(") ~ ConditionExpression ~ WS(")") |
      Expression ~ ComparisonOp ~ Expression ~> Query.ConditionExpression.Comparison.apply
  }

  def ComparisonOp: Rule1[Query.ComparisonOp] = rule {
    WS("==") ~ push(Query.ComparisonOp.Eq) |
      WS("!=") ~ push(Query.ComparisonOp.Neq) |
      WS("<") ~ push(Query.ComparisonOp.Lt) |
      WS("<=") ~ push(Query.ComparisonOp.Leq) |
      WS(">") ~ push(Query.ComparisonOp.Gt) |
      WS(">=") ~ push(Query.ComparisonOp.Geq)
  }

  def MathExpression: Rule1[Query.Expression] = rule {
    MathTerm ~ zeroOrMore(
      WS("+") ~ MathTerm ~> (Query.MathExpression(_, Query.MathOp.Add, _)) |
        WS("-") ~ MathTerm ~> (Query.MathExpression(_, Query.MathOp.Sub, _))
    )
  }

  def MathTerm: Rule1[Query.Expression] = rule {
    MathFactor ~ zeroOrMore(
      WS("*") ~ MathFactor ~> (Query.MathExpression(_, Query.MathOp.Mul, _)) |
        WS("/") ~ MathFactor ~> (Query.MathExpression(_, Query.MathOp.Div, _))
    )
  }

  def MathFactor: Rule1[Query.Expression] = rule {
    WS("(") ~ MathExpression ~ WS(")") |
      NumberLiteral |
      KeyPathLiteral
  }

  def Operator: Rule1[Query.Operator] = rule {
    Filter |
      Block |
      Remove |
      OrderBy |
      Limit
  }

  def Filter: Rule1[Query.Operator.Filter] = rule {
    Keywords("filter", "where") ~ ConditionExpression ~> Query.Operator.Filter.apply
  }

  def Block: Rule1[Query.Operator.Filter] = rule {
    Keyword("block") ~ ConditionExpression ~> Query.ConditionExpression.Not.apply ~> Query.Operator.Filter.apply
  }

  def Remove: Rule1[Query.Operator.Remove] = rule {
    Keywords("remove") ~ oneOrMore(KeyPathLiteral).separatedBy(WS(',')) ~> Query.Operator.Remove.apply
  }

  def OrderBy: Rule1[Query.Operator.OrderBy] = rule {
    Keyword("orderby") ~ oneOrMore(KeyPathLiteral).separatedBy(WS(',')) ~ Sort ~> Query.Operator.OrderBy.apply
  }

  def Limit: Rule1[Query.Operator.Limit] = rule {
    Keyword("limit") ~ capture(oneOrMore(CharPredicate.Digit)) ~ WS ~> (l => Query.Operator.Limit(l.toLong))
  }

  def Sort: Rule1[Query.Sort] =
    rule {
      "asc" ~ push(Query.Sort.Asc) |
        "desc" ~ push(Query.Sort.Desc)
    }
}

case class Query(source: Query.Source, operators: Seq[Query.Operator])

object Query {

  enum Source {
    case Logs
    case Spans
    case Custom(enrichment: String)
  }

  object Source {

    def fromString(str: String): Source =
      str match {
        case "logs"  => Source.Logs
        case "spans" => Source.Spans
        case custom  => Source.Custom(custom)
      }
  }

  sealed trait Expression

  enum Field {
    case Metadata(field: String)
    case Labels(field: String)
    case UserData(field: String)
  }

  object Field {
    def apply(`type`: String, field: String): Field =
      `type` match {
        case "m" => Field.Metadata(field)
        case "l" => Field.Labels(field)
        case "d" => Field.UserData(field)
        case _   => Field.UserData(field)
      }
  }

  sealed trait Literal extends Expression
  object Literal {
    case class StringValue(value: String)     extends Literal
    case class NumberValue(value: BigDecimal) extends Literal
    case class RegExp(value: String)          extends Literal
    case class KeyPath(field: Field)          extends Literal
  }

  sealed trait ConditionExpression
  object ConditionExpression {
    case class And(factors: Seq[ConditionExpression]) extends ConditionExpression
    case class Or(terms: Seq[ConditionExpression])    extends ConditionExpression

    case class Not(expr: ConditionExpression)                                    extends ConditionExpression
    case class Comparison(left: Expression, op: ComparisonOp, right: Expression) extends ConditionExpression
  }
  enum ComparisonOp {
    case Eq, Neq, Lt, Leq, Gt, Geq
  }

  case class MathExpression(left: Expression, op: MathOp, right: Expression) extends Expression
  enum MathOp {
    case Add, Sub, Mul, Div
  }

  enum ValueType {
    case Boolean, Number, String, Timestamp
  }

  enum Sort {
    case Asc, Desc
  }

  sealed trait Operator
  object Operator {
    case class Filter(condition: ConditionExpression)            extends Operator
    case class Remove(fields: Seq[Literal.KeyPath])              extends Operator
    case class OrderBy(fields: Seq[Literal.KeyPath], sort: Sort) extends Operator
    case class Limit(count: Long)                                extends Operator
  }
}

var parser =
  QueryParser(
    """|source logs |
       |  filter $d.result == 'success' && ($d.region != 'eu-west-1' || $d.region == 'us-east-1') |
       |  orderby $m.severity, $m.timestamp desc |
       |  limit 42
       |""".stripMargin
  )
parser.Parser.run() match {
  case scala.util.Failure(cause: ParseError) => println(parser.formatError(cause))
  case scala.util.Failure(cause)             => throw cause
  case scala.util.Success(obtained) =>
    import Query.*
    import munit.Assertions.*

    val expected =
      Query(
        Source.Logs,
        Vector(
          Operator.Filter(
            ConditionExpression.And(
              Vector(
                ConditionExpression.Comparison(
                  Literal.KeyPath(Field.UserData("result")),
                  ComparisonOp.Eq,
                  Literal.StringValue("success")
                ),
                ConditionExpression.Or(
                  Vector(
                    ConditionExpression.Comparison(
                      Literal.KeyPath(Field.UserData("region")),
                      ComparisonOp.Neq,
                      Literal.StringValue("eu-west-1")
                    ),
                    ConditionExpression.Comparison(
                      Literal.KeyPath(Field.UserData("region")),
                      ComparisonOp.Eq,
                      Literal.StringValue("us-east-1")
                    )
                  )
                )
              )
            )
          ),
          Operator.OrderBy(
            Vector(
              Literal.KeyPath(Field.Metadata("severity")),
              Literal.KeyPath(Field.Metadata("timestamp"))
            ),
            Sort.Desc
          ),
          Operator.Limit(42L)
        )
      )

    assertEquals(obtained, expected)
}

parser = QueryParser(
  """|source logs |
       |  filter $d.result == 1 + 42 * 13 + $d.constant |
       |  limit 42
       |""".stripMargin
)
parser.Parser.run() match {
  case scala.util.Failure(cause: ParseError) => println(parser.formatError(cause))
  case scala.util.Failure(cause)             => throw cause
  case scala.util.Success(obtained) =>
    import Query.*
    import munit.Assertions.*

    val expected =
      Query(
        Source.Logs,
        Vector(
          Operator.Filter(
            ConditionExpression.Comparison(
              Literal.KeyPath(Field.UserData("result")),
              ComparisonOp.Eq,
              MathExpression(
                MathExpression(
                  Literal.NumberValue(1),
                  MathOp.Add,
                  MathExpression(
                    Literal.NumberValue(42),
                    MathOp.Mul,
                    Literal.NumberValue(13)
                  )
                ),
                MathOp.Add,
                Literal.KeyPath(Field.UserData("constant"))
              )
            )
          ),
          Operator.Limit(count = 42L)
        )
      )

    assertEquals(obtained, expected)
}
