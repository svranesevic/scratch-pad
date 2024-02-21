import zio.schema.*
import munit.Assertions.*
import pprint.pprintln

case class Account(name: String, address: Address, secondaryAddresses: List[Address], notes: List[String])
object Account {

  given schema: Schema.CaseClass4[String, Address, List[Address], List[String], Account] =
    DeriveSchema.gen[Account]

  val (name, address, secondaryAddresses, notes) =
    schema.makeAccessors(QueryModelBuilder)
}

case class Address(country: String, city: String)
object Address {
  given schema: Schema.CaseClass2[String, String, Address] =
    DeriveSchema.gen[Address]

  val (country, city) =
    schema.makeAccessors(QueryModelBuilder)
}

object QueryModelBuilder extends AccessorBuilder {

  override type Lens[F, S, A]   = QueryField.With[S, A]
  override type Traversal[S, A] = Unit
  override type Prism[F, S, A]  = Unit

  override def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[S, A]): Lens[F, S, A] =
    new QueryField(term.name) {
      override type Record = S
      override type Value  = A
    }

  override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] =
    ()

  override def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[S, A]): Prism[F, S, A] =
    ()
}

enum Predicate {
  case Eq[A](field: QueryField, value: A)
  case ContainsAny[A](field: QueryField, values: Set[A])
  case In[A](field: QueryField, values: Set[A])

  case And(left: Predicate, right: Predicate)
  case Or(left: Predicate, right: Predicate)

  def &&(that: Predicate): Predicate = And(this, that)
  def ||(that: Predicate): Predicate = Or(this, that)
}

case class QueryField(val name: String) { self =>

  type Record
  type Value

  def =:=(value: Value): Predicate =
    Predicate.Eq(this, value)

  def in(value: Set[Value]): Predicate =
    Predicate.In(this, value)

  def /[Value1](that: QueryField.With[Value, Value1]): QueryField.With[Record, Value1] =
    new QueryField(s"$name.${that.name}") {
      override type Record = self.Record
      override type Value  = Value1
    }

  def /[Item, Value1](
    that: QueryField.With[Item, Value1]
  )(using Value <:< Iterable[Item]): QueryField.With[Record, Value1] =
    new QueryField(s"$name.${that.name}") {
      override type Record = self.Record
      override type Value  = Value1
    }

  def contains[Value1](value: Value1)(using Value <:< Iterable[Value1]): Predicate =
    Predicate.ContainsAny(this, Set(value))
}
object QueryField {
  type With[Record1, Value1] =
    QueryField {
      type Record = Record1
      type Value  = Value1
    }

  type Of[Value1] =
    QueryField {
      type Value = Value1
    }
}

// Query by "primitive type" field
assertEquals(
  Account.name =:= "Joe",
  Predicate.Eq(QueryField("name"), "Joe")
)

// Query by "record type" field
assertEquals(
  Account.address =:= Address("RS", "BG"),
  Predicate.Eq(QueryField("address"), Address("RS", "BG"))
)
// Type-safe selection of inner field(s)
assertEquals(
  Account.address / Address.country =:= "RS",
  Predicate.Eq(QueryField("address.country"), "RS")
)

// Query by collection field, where collection item is "primitive type"
assertEquals(
  Account.notes =:= List("Gold membership"),
  Predicate.Eq(QueryField("notes"), List("Gold membership"))
)
assertEquals(
  Account.notes.contains("Gold membership"),
  Predicate.ContainsAny(QueryField("notes"), Set("Gold membership"))
)

// Query by collection field, where collection item is of "record type"
assertEquals(
  Account.secondaryAddresses =:= List(Address("RS", "BG")),
  Predicate.Eq(QueryField("secondaryAddresses"), List(Address("RS", "BG")))
)
assertEquals(
  Account.secondaryAddresses.contains(Address("RS", "BG")),
  Predicate.ContainsAny(QueryField("secondaryAddresses"), Set(Address("RS", "BG")))
)
// Type-safe selection of inner field(s)
assertEquals(
  Account.secondaryAddresses / Address.country =:= "RS",
  Predicate.Eq(QueryField("secondaryAddresses.country"), "RS")
)
assertEquals(
  Account.secondaryAddresses / Address.country =:= "RS" && Account.secondaryAddresses / Address.city =:= "BG",
  Predicate.And(
    Predicate.Eq(QueryField("secondaryAddresses.country"), "RS"),
    Predicate.Eq(QueryField("secondaryAddresses.city"), "BG")
  )
)
