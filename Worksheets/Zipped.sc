// Scala 3 Match Types: https://dotty.epfl.ch/docs/reference/new-types/match-types.html

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
    case Either[l, r] => Either[L | l, Zipped.Type[R, r]]
    case _            => Either[L, Zipped.Type[R, B]]

def zipEither[L, R, B](a: Either[L, R], b: B): ZipEither[L, R, B] =
  b match
    case b: Either[l, r] => a.flatMap(a => b.map(b => Zipped(a, b)))
    case _               => a.map(a => Zipped(a, b))

object Zipped {
  type Type[A, B] =
    A match
      case Unit         => B
      case Tuple        => ZipTuple[A, B]
      case Either[l, r] => ZipEither[l, r, B]
      case _            => ZipAny[A, B]

  def apply[A, B](a: A, b: B): Zipped.Type[A, B] =
    a match {
      case _: Unit         => b
      case a: Tuple        => zipTuple(a, b)
      case a: Either[l, r] => zipEither[l, r, B](a, b)
      case _               => zipAny(a, b)
    }
}
