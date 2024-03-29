import zio.*
import zio.schema.*
import munit.Assertions.*
import pprint.pprintln

enum Predicate[S] {

  case Eq[S, A](field: Field[S, A], value: A)       extends Predicate[S]
  case In[S, A](field: Field[S, A], values: Set[A]) extends Predicate[S]

  case ContainsAny[S, A](field: Field[S, Iterable[A]], values: Set[A])       extends Predicate[S]
  case Contains[S, A](field: Field[S, Iterable[A]], predicate: Predicate[A]) extends Predicate[S]

  case And(left: Predicate[S], right: Predicate[S])
  case Or(left: Predicate[S], right: Predicate[S])

  // Ops
  def &&(that: Predicate[S]): Predicate[S] = And(this, that)
  def ||(that: Predicate[S]): Predicate[S] = Or(this, that)

  def prefix[S1](prefix: Field[S1, S]): Predicate[S1] =
    this match {
      case eq: Eq[?, ?] => eq.copy(field = eq.field.prefix(prefix))
      case in: In[?, ?] => in.copy(field = in.field.prefix(prefix))

      case containsAny: ContainsAny[?, ?] => containsAny.copy(field = containsAny.field.prefix(prefix))
      case contains: Contains[?, ?]       => contains.copy(field = contains.field.prefix(prefix))

      case And(left, right) => And(left.prefix(prefix), right.prefix(prefix))
      case Or(left, right)  => Or(left.prefix(prefix), right.prefix(prefix))
    }
}

case class Field[S, A](path: NonEmptyChunk[String]) { self =>

  def widen[A1](using A <:< A1): Field[S, A1] =
    self.asInstanceOf[Field[S, A1]]

  def prefix[S1](field: Field[S1, S]): Field[S1, A] =
    field / self

  def =:=(value: A): Predicate[S] =
    Predicate.Eq(self, value)

  def in(value: Set[A]): Predicate[S] =
    Predicate.In(self, value)

  def /[A1](that: Field[A, A1]): Field[S, A1] =
    new Field[S, A1](self.path ++ that.path)

  def where(predicate: Predicate[A]): Predicate[S] =
    predicate.prefix(self)
}
object Field {

  def apply[S, A](path: String, paths: String*): Field[S, A] =
    Field(NonEmptyChunk(path, paths*))

  extension [S, A1, A <: Iterable[A1]](self: Field[S, A]) {
    def /[A2](that: Field[A1, A2]): Field[S, A2] =
      Field(self.path ++ that.path)

    def contains(that: A1): Predicate[S] =
      Predicate.ContainsAny(self.widen, Set(that))

    // Shadow `Field#where` which would accept ill-formed predicate of Iterable[A1].
    // Signals that different fields, eg. CollectionField, should be introduced,
    // but omitted here for sake of simplicity
    def where(predicate: Predicate[A1]): Predicate[S] =
      contains(predicate)

    def contains(predicate: Predicate[A1]): Predicate[S] =
      Predicate.Contains(self.widen, predicate)
  }
}

object QueryModelBuilder extends AccessorBuilder {

  override type Lens[F, S, A]   = Field[S, A]
  override type Traversal[S, A] = Unit
  override type Prism[F, S, A]  = Unit

  override def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[S, A]): Lens[F, S, A] =
    Field[S, A](NonEmptyChunk(term.name))

  override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] =
    ()

  override def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[S, A]): Prism[F, S, A] =
    ()
}

// Usage
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

import Predicate.*

// Query by "primitive type" field
assertEquals(
  Account.name =:= "Joe",
  Eq(Field("name"), "Joe")
)

// Query by "record type" field
assertEquals(
  Account.address =:= Address("RS", "BG"),
  Eq(Field("address"), Address("RS", "BG"))
)
// Type-safe selection of inner field(s)
assertEquals(
  Account.address / Address.country =:= "RS",
  Eq(Field("address", "country"), "RS")
)

// Query by collection field, where collection item is "primitive type"
assertEquals(
  Account.notes =:= List("Gold membership"),
  Eq(Field("notes"), List("Gold membership"))
)
assertEquals(
  Account.notes.contains("Gold membership"),
  ContainsAny(Field("notes"), Set("Gold membership"))
)

// Query by collection field, where collection item is of "record type"
assertEquals(
  Account.secondaryAddresses =:= List(Address("RS", "BG")),
  Eq(Field("secondaryAddresses"), List(Address("RS", "BG")))
)
assertEquals(
  Account.secondaryAddresses.contains(Address("RS", "BG")),
  ContainsAny(Field("secondaryAddresses"), Set(Address("RS", "BG")))
)
// Type-safe selection of inner field(s)
assertEquals(
  Account.secondaryAddresses / Address.country =:= "RS",
  Eq(Field("secondaryAddresses", "country"), "RS")
)
assertEquals(
  Account.secondaryAddresses / Address.country =:= "RS" &&
    Account.secondaryAddresses / Address.city =:= "BG",
  And(
    Eq(Field("secondaryAddresses", "country"), "RS"),
    Eq(Field("secondaryAddresses", "city"), "BG")
  )
)

// Reuse predicate / Ergonomic predicates on inner field(s)
val addressPredicate: Predicate[Address] = Address.country =:= "RS" && Address.city =:= "BG"
assertEquals(
  Account.address.where(addressPredicate),
  And(
    Eq(Field("address", "country"), "RS"),
    Eq(Field("address", "city"), "BG")
  )
)

assertEquals(
  Account.secondaryAddresses.contains(addressPredicate),
  Contains(
    Field("secondaryAddresses"),
    And(
      Eq(Field("country"), "RS"),
      Eq(Field("city"), "BG")
    )
  )
)
