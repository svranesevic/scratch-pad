import zio.*
import zio.schema.*
import munit.Assertions.*
import pprint.pprintln

sealed trait Plan[-Q, +R] { self =>

  import Plan.*

  def widen[R1](implicit ev: R <:< R1): Plan[Q, R1] =
    self.asInstanceOf[Plan[Q, R1]]

  def filter[R1](predicate: Plan[R1, Boolean])(using R <:< List[R1]): Plan[Q, List[R1]] =
    Filter(self.widen, predicate)

  def limit[R1](count: Int)(using R <:< List[R1]): Plan[Q, List[R1]] =
    Limit(self.widen, count)

  def project[R1, R2](projection: Plan[R1, R2])(using R <:< List[R1]): Plan[Q, List[R2]] =
    Project(self.widen, projection)

  def groupBy[R1, K](groupByKey: Plan[R1, K])(using R <:< List[R1]): Plan[Q, Map[K, List[R1]]] =
    GroupBy(self.widen, groupByKey)

  def groupAggregate[R1, R2, R3, K](
    groupByKey: Plan[R1, K],
    agg1: Plan[List[R1], R2],
    agg2: Plan[List[R1], R3]
  )(using
    R <:< List[R1]
  ): Plan[Q, List[(K, R2, R3)]] =
    GroupAggregation(self.widen, groupByKey, agg1, agg2)
}
object Plan {

  case class Filter[Q, R](source: Plan[Q, List[R]], predicate: Plan[R, Boolean]) extends Plan[Q, List[R]]

  case class Limit[Q, R](source: Plan[Q, List[R]], count: Int) extends Plan[Q, List[R]]

  case class Project[Q, R1, R2](
    source: Plan[Q, List[R1]],
    projection: Plan[R1, R2]
  ) extends Plan[Q, List[R2]]

  case class GroupBy[Q, R, K](source: Plan[Q, List[R]], groupBykey: Plan[R, K]) extends Plan[Q, Map[K, List[R]]]

  case class GroupAggregation[Q, R, R2, R3, K](
    source: Plan[Q, List[R]],
    groupByKey: Plan[R, K],
    agg1: Plan[List[R], R2],
    agg2: Plan[List[R], R3]
  ) extends Plan[Q, List[(K, R2, R3)]]

  case class Max[Q, R](source: Plan[Q, R]) extends Plan[List[Q], R]

  case class Average[Q, R](source: Plan[Q, R]) extends Plan[List[Q], R]
}

trait DataSource[R] extends Plan[Nothing, List[R]]

// case class Field[S, A](product: Schema.Record[S], term: Schema.Field[S, A]) extends Plan[S, A] { self =>
case class Field[S, A](path: NonEmptyChunk[String]) extends Plan[S, A] {
  self =>

  def =:=(that: A): Predicate[S] =
    Predicate.Eq(self, that)

  def /[A1](that: Field[A, A1]): Field[S, A1] =
    Field(self.path ++ that.path)

  def max: Plan[List[S], A] = Plan.Max(self)

  def avg(using Numeric[A]): Plan[List[S], A] = Plan.Average(self)
}

object Field {

  def apply[S, A](path: String, paths: String*): Field[S, A] =
    Field(NonEmptyChunk(path, paths*))

  extension [S, A1, A <: Iterable[A1]](self: Field[S, A]) {
    def contains(that: A1)(using A <:< Iterable[A1]): Predicate[S] =
      Predicate.ContainsAny(self, Set(that))

    def /[A2](that: Field[A1, A2]): Field[S, A2] =
      Field(self.path ++ that.path)
  }
}

object QueryModelBuilder extends AccessorBuilder {

  override type Lens[F, S, A]   = Field[S, A]
  override type Traversal[S, A] = Unit
  override type Prism[F, S, A]  = Unit

  override def makeLens[F, S, A](
    product: Schema.Record[S],
    term: Schema.Field[S, A]
  ): Lens[F, S, A] =
    // Plan.Field(product, term)
    Field(NonEmptyChunk(term.name))

  override def makeTraversal[S, A](
    collection: Schema.Collection[S, A],
    element: Schema[A]
  ): Traversal[S, A] =
    ()

  override def makePrism[F, S, A](
    sum: Schema.Enum[S],
    term: Schema.Case[S, A]
  ): Prism[F, S, A] =
    ()
}

sealed trait Predicate[S] extends Plan[S, Boolean] { self =>

  def &&(that: Predicate[S]): Predicate.And[S] = Predicate.And(self, that)
  def ||(that: Predicate[S]): Predicate.Or[S]  = Predicate.Or(self, that)
}
object Predicate {

  case class Eq[S, A](plan: Plan[S, A], value: A) extends Predicate[S]

  case class ContainsAny[S, A](plan: Plan[S, A], values: Set[A]) extends Predicate[S]

  case class And[S](left: Predicate[S], right: Predicate[S]) extends Predicate[S]
  case class Or[S](left: Predicate[S], right: Predicate[S])  extends Predicate[S]
}

// Usage
case class Account(
  name: String,
  balance: BigDecimal,
  address: Address,
  secondaryAddresses: List[Address],
  notes: List[String]
)
object Account {

  given schema: Schema.CaseClass5[String, BigDecimal, Address, List[Address], List[String], Account] =
    DeriveSchema.gen[Account]

  val (name, balance, address, secondaryAddresses, notes) =
    schema.makeAccessors(QueryModelBuilder)
}

case class Address(country: String, city: String)
object Address {
  given schema: Schema.CaseClass2[String, String, Address] =
    DeriveSchema.gen[Address]

  val (country, city) =
    schema.makeAccessors(QueryModelBuilder)
}

case object AccountDataSource extends DataSource[Account]

import Plan.*
import Predicate.*

// Query by "primitive type" field
assertEquals(
  AccountDataSource.filter(Account.name =:= "Joe"),
  Filter(
    AccountDataSource,
    Eq(Field("name"), "Joe")
  )
)

// Query by "record type" field
assertEquals(
  AccountDataSource.filter(Account.address =:= Address("RS", "BG")),
  Filter(
    AccountDataSource,
    Eq(Field("address"), Address("RS", "BG"))
  )
)
// Type-safe selection of inner field(s)
assertEquals(
  AccountDataSource.filter(Account.address / Address.country =:= "RS"),
  Filter(
    AccountDataSource,
    Eq(Field("address", "country"), "RS")
  )
)

// Query by collection field, where collection item is "primitive type"
assertEquals(
  AccountDataSource.filter(Account.notes =:= List("Gold membership")),
  Filter(
    AccountDataSource,
    Eq(Field("notes"), List("Gold membership"))
  )
)
assertEquals(
  AccountDataSource.filter(Account.notes.contains("Gold membership")),
  Filter(
    AccountDataSource,
    ContainsAny(Field("notes"), Set("Gold membership"))
  )
)

// Query by collection field, where collection item is of "record type"
assertEquals(
  AccountDataSource.filter(
    Account.secondaryAddresses =:= List(Address("RS", "BG"))
  ),
  Filter(
    AccountDataSource,
    Eq(Field("secondaryAddresses"), List(Address("RS", "BG")))
  )
)
assertEquals(
  AccountDataSource.filter(
    Account.secondaryAddresses.contains(Address("RS", "BG"))
  ),
  Filter(
    AccountDataSource,
    ContainsAny(
      Field("secondaryAddresses"),
      Set(Address("RS", "BG"))
    )
  )
)
// Type-safe selection of inner field(s)
assertEquals(
  AccountDataSource.filter(
    Account.secondaryAddresses / Address.country =:= "RS"
  ),
  Filter(
    AccountDataSource,
    Eq(Field("secondaryAddresses", "country"), "RS")
  )
)
assertEquals(
  AccountDataSource.filter(
    Account.secondaryAddresses / Address.country =:= "RS" &&
      Account.secondaryAddresses / Address.city =:= "BG"
  ),
  Filter(
    AccountDataSource,
    And(
      Eq(Field("secondaryAddresses", "country"), "RS"),
      Eq(Field("secondaryAddresses", "city"), "BG")
    )
  )
)

// Grand finale
assertEquals(
  AccountDataSource
    .filter(
      Account.secondaryAddresses / Address.country =:= "RS" &&
        Account.secondaryAddresses / Address.city =:= "BG"
    )
    .groupAggregate(Account.address, Account.balance.max, Account.balance.avg),
  GroupAggregation(
    Filter(
      AccountDataSource,
      And(
        Eq(Field("secondaryAddresses", "country"), "RS"),
        Eq(Field("secondaryAddresses", "city"), "BG")
      )
    ),
    groupByKey = Field("address"),
    agg1 = Max(Field("balance")),
    agg2 = Average(Field("balance"))
  )
)
