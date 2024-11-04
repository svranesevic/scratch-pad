sealed trait Eval[+A] {

  import Eval.*

  @scala.annotation.tailrec
  protected final def step: Either[() => Eval[A], A] = this match {
    case Done(value) => Right(value)
    case Defer(eval) => Left(eval)
    case FlatMap(eval, cont) =>
      eval match {
        case Done(value)           => cont(value).step
        case Defer(eval)           => Left(() => FlatMap(eval(), cont))
        case FlatMap(eval2, cont2) => FlatMap(eval2, x => FlatMap(cont2(x), cont)).step
      }
  }

  @scala.annotation.tailrec
  final def value: A = step match {
    case Right(value) => value
    case Left(eval)   => eval().value
  }

  final def map[B](f: A => B): Eval[B] = this match {
    case Done(a)       => Done(f(a))
    case Defer(_)      => FlatMap(this, a => Done(f(a)))
    case FlatMap(_, _) => FlatMap(this, a => Done(f(a)))
  }

  final def flatMap[B](f: A => Eval[B]): Eval[B] = this match {
    case Done(a)       => f(a)
    case Defer(_)      => FlatMap(this, a => f(a))
    case FlatMap(_, _) => FlatMap(this, a => f(a))
  }
}

object Eval {

  def now[A](a: => A): Eval[A]         = Done(a)
  def always[A](a: => A): Eval[A]      = Defer(() => now(a))
  def defer[A](a: => Eval[A]): Eval[A] = Defer(() => a)

  val True: Eval[Boolean]  = Done(true)
  val False: Eval[Boolean] = Done(false)

  private final case class Done[A](a: A)                                    extends Eval[A]
  private final case class Defer[A](fn: () => Eval[A])                      extends Eval[A]
  private final case class FlatMap[A, B](eval: Eval[A], cont: A => Eval[B]) extends Eval[B]
}
