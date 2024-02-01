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
    atomic(ignoreCase(s.toLowerCase)) ~ oneOrMore(anyOf(" \n\r\t\f")).named("space")
  }

  def Keywords(s: String*): Rule0 = rule {
    s.map(Keyword).reduce((l, r) => rule(l | r))
  }

  // Grammar

  def QueryStatement: Rule1[Query] = rule {
    WS ~ Source ~ zeroOrMore(Pipe ~ Operator) ~> Query.apply
  }

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
    ArithmeticExpression |
      StringLiteral |
      NumberLiteral |
      RegExpLiteral |
      KeyPathLiteral
  }

  def LogicalExpression: Rule1[Query.LogicalExpression] = rule {
    oneOrMore(LogicalTerms).separatedBy(WS("||")) ~> {
      case Seq(expr) => expr
      case exprs     => Query.LogicalExpression.Or(exprs)
    }
  }

  def LogicalTerms: Rule1[Query.LogicalExpression] = rule {
    oneOrMore(LogicalFactors).separatedBy(WS("&&")) ~> {
      case Seq(expr) => expr
      case exprs     => Query.LogicalExpression.And(exprs)
    }
  }

  def LogicalFactors: Rule1[Query.LogicalExpression] = rule {
    WS("(") ~ LogicalExpression ~ WS(")") |
      Expression ~ RelationOp ~ Expression ~> Query.LogicalExpression.Relation.apply
  }

  def RelationOp: Rule1[Query.RelationOp] = rule {
    WS("==") ~ push(Query.RelationOp.Eq) |
      WS("!=") ~ push(Query.RelationOp.Neq) |
      WS("<") ~ push(Query.RelationOp.Lt) |
      WS("<=") ~ push(Query.RelationOp.Leq) |
      WS(">") ~ push(Query.RelationOp.Gt) |
      WS(">=") ~ push(Query.RelationOp.Geq)
  }

  def ArithmeticExpression: Rule1[Query.Expression] = rule {
    ArithmeticTerm ~ zeroOrMore(
      WS("+") ~ ArithmeticTerm ~> (Query.ArithmeticExpression(_, Query.ArithmeticOp.Add, _)) |
        WS("-") ~ ArithmeticTerm ~> (Query.ArithmeticExpression(_, Query.ArithmeticOp.Sub, _))
    )
  }

  def ArithmeticTerm: Rule1[Query.Expression] = rule {
    ArithmeticFactor ~ zeroOrMore(
      WS("*") ~ ArithmeticFactor ~> (Query.ArithmeticExpression(_, Query.ArithmeticOp.Mul, _)) |
        WS("/") ~ ArithmeticFactor ~> (Query.ArithmeticExpression(_, Query.ArithmeticOp.Div, _))
    )
  }

  def ArithmeticFactor: Rule1[Query.Expression] = rule {
    WS("(") ~ ArithmeticExpression ~ WS(")") |
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
    Keywords("filter", "where") ~ LogicalExpression ~> Query.Operator.Filter.apply
  }

  def Block: Rule1[Query.Operator.Filter] = rule {
    Keyword("block") ~ LogicalExpression ~> Query.LogicalExpression.Not.apply ~> Query.Operator.Filter.apply
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
      Keyword("asc") ~ push(Query.Sort.Asc) |
        Keyword("desc") ~ push(Query.Sort.Desc)
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
        case "m" | "metadata" => Field.Metadata(field)
        case "l" | "labels"   => Field.Labels(field)
        case "d" | "data"     => Field.UserData(field)
        case _                => Field.UserData(field)
      }
  }

  sealed trait Literal extends Expression
  object Literal {
    case class StringValue(value: String)     extends Literal
    case class NumberValue(value: BigDecimal) extends Literal
    case class RegExp(value: String)          extends Literal
    case class KeyPath(field: Field)          extends Literal
  }

  sealed trait LogicalExpression extends Expression
  object LogicalExpression {
    case class Or(terms: Seq[LogicalExpression])    extends LogicalExpression
    case class And(factors: Seq[LogicalExpression]) extends LogicalExpression
    case class Not(expr: LogicalExpression)         extends LogicalExpression

    case class Relation(left: Expression, op: RelationOp, right: Expression) extends LogicalExpression
  }
  enum RelationOp {
    case Eq, Neq, Lt, Leq, Gt, Geq
  }

  case class ArithmeticExpression(left: Expression, op: ArithmeticOp, right: Expression) extends Expression
  enum ArithmeticOp {
    case Add, Sub, Mul, Div
  }

  case class Cast(expr: Expression, toType: ValueType) extends Expression

  enum ValueType {
    case Boolean, Number, String, Timestamp
  }

  enum Sort {
    case Asc, Desc
  }

  sealed trait Operator
  object Operator {
    case class Filter(condition: LogicalExpression)              extends Operator
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
            LogicalExpression.And(
              Vector(
                LogicalExpression.Relation(
                  Literal.KeyPath(Field.UserData("result")),
                  RelationOp.Eq,
                  Literal.StringValue("success")
                ),
                LogicalExpression.Or(
                  Vector(
                    LogicalExpression.Relation(
                      Literal.KeyPath(Field.UserData("region")),
                      RelationOp.Neq,
                      Literal.StringValue("eu-west-1")
                    ),
                    LogicalExpression.Relation(
                      Literal.KeyPath(Field.UserData("region")),
                      RelationOp.Eq,
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
            LogicalExpression.Relation(
              Literal.KeyPath(Field.UserData("result")),
              RelationOp.Eq,
              ArithmeticExpression(
                ArithmeticExpression(
                  Literal.NumberValue(1),
                  ArithmeticOp.Add,
                  ArithmeticExpression(
                    Literal.NumberValue(42),
                    ArithmeticOp.Mul,
                    Literal.NumberValue(13)
                  )
                ),
                ArithmeticOp.Add,
                Literal.KeyPath(Field.UserData("constant"))
              )
            )
          ),
          Operator.Limit(42L)
        )
      )

    assertEquals(obtained, expected)
}
