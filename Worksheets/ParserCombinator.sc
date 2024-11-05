import Zipped.*
import Trampoline.*

trait Parser[+A] { self =>

  import Parser.*

  def eval(input: String): Eval[Either[String, (A, String)]]

  def parse(input: String): Either[String, (A, String)] =
    eval(input).value

  def map[B](f: A => B): Parser[B] = { input =>
    self.eval(input).map(_.map { case (a, left) => f(a) -> left })
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = { input =>
    Eval.defer {
      self.eval(input).flatMap {
        case Left(msg)        => Eval.now(Left(msg))
        case Right((a, left)) => f(a).eval(left)
      }
    }
  }

  def as[B](b: => B): Parser[B] =
    map(_ => b)

  def zipWith[B, C](that: => Parser[B])(f: (A, B) => C): Parser[C] =
    this.flatMap(l => that.map(r => f(l, r)))

  def zip[B, A1 >: A, B1 >: B](that: => Parser[B]): Parser[Zipped.Type[A1, B1]] =
    this.zipWith(that)(Zipped.apply)

  def or[A1 >: A](that: => Parser[A1]): Parser[A1] = Parser.eval { input =>
    parse(input) match {
      case Left(_) => that.eval(input)
      case r       => Eval.now(r)
    }
  }

  def opt: Parser[Option[A]] = Parser { input =>
    parse(input) match {
      case Left(_)          => Right(None -> input)
      case Right((a, left)) => Right(Some(a) -> left)
    }
  }

  def many: Parser[List[A]] = Parser { input =>
    @scala.annotation.tailrec
    def loop(in: String, acc: List[A]): (List[A], String) = this.parse(in) match {
      case Right((a, left)) => loop(left, acc :+ a)
      case _: Left[?, ?]    => acc -> in
    }
    val (results, remaining) = loop(input, List.empty)
    Right((results, remaining))
  }

  def many1: Parser[List[A]] =
    this.zipWith(this.many)(_ :: _)

  def many(sep: Parser[?]): Parser[List[A]] =
    many1(sep) | const(Nil)

  def many1(sep: Parser[?]): Parser[List[A]] =
    this.zipWith((sep *> this).many)(_ :: _)
}

object Parser {

  def apply[A](f: String => Either[String, (A, String)]): Parser[A] =
    input => Eval.now(f(input))

  def defer[A](f: => Parser[A]): Parser[A] =
    f.eval(_)

  def eval[A](f: String => Eval[Either[String, (A, String)]]): Parser[A] =
    f(_)

  extension [A](p: Parser[A]) {
    infix def ~[B](that: => Parser[B]): Parser[Zipped.Type[A, B]] =
      p.zip(that)

    infix def ~~>[B](f: A => Parser[B]): Parser[B] =
      p.flatMap(f)

    infix def |[B >: A](that: => Parser[B]): Parser[B] =
      p.or(that)

    infix def ~>[B](b: => B): Parser[B] =
      p.as(b)

    infix def ~>[B](f: A => B): Parser[B] =
      p.map(f)

    infix def *>[B](that: Parser[B]): Parser[B] =
      p.flatMap(_ => that)

    infix def <*(that: Parser[?]): Parser[A] =
      p.flatMap(l => that.as(l))
  }

  // Conversions
  given Conversion[String, Parser[String]] = Parser.str
  given Conversion[Char, Parser[Char]]     = Parser.char

  // DSL
  def const[A](a: A): Parser[A] = Parser(input => Right(a -> input))

  def str(str: String): Parser[String] = Parser { input =>
    if (input.startsWith(str)) Right(str -> input.drop(str.length))
    else Left(s"Expected: $str, got $input")
  }

  def char(c: Char): Parser[Char] =
    str(c.toString).map(_.head)

  def regex(pattern: String): Parser[String] = {
    val regex = new scala.util.matching.Regex(s"^($pattern).*")
    Parser { input =>
      regex.findFirstMatchIn(input) match {
        case None =>
          Left(s"Expected regex: $pattern, got $input")
        case Some(m) =>
          val matchh = m.group(1)
          Right(matchh -> input.drop(matchh.length))
      }
    }
  }

  def digit: Parser[Int] =
    regex("[0-9]").map(_.toInt)

  def oneOf[A](p: Parser[A], ps: Parser[A]*): Parser[A] =
    oneOf(p +: ps)

  def oneOf[A](ps: Iterable[Parser[A]]): Parser[A] = Parser { input =>
    val results = ps.toVector.map(_.parse(input))

    val success = results
      .collect { case Right(a, left) => a -> left }
      .sortBy(_._2.size)
      .map(Right.apply)

    success.headOption
      .orElse(results.headOption)
      .getOrElse(throw new IllegalStateException("oneOf: Must have at least one Parser supplied"))
  }
}
