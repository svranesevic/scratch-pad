sealed trait Eval[+A] {

  import Eval.*

  def value: A = evaluate

  @scala.annotation.tailrec
  protected final def step: Either[() => Eval[A], A] = this match {
    case Now(value)    => Right(value)
    case Always(eval)  => Left(eval)
    case Memoize(eval) => Left(eval)
    case FlatMap(eval, cont) =>
      eval match {
        case Now(value)            => cont(value).step
        case Memoize(eval)         => Left(() => FlatMap(eval(), cont))
        case Always(eval)          => Left(() => FlatMap(eval(), cont))
        case FlatMap(eval2, cont2) => FlatMap(eval2, x => FlatMap(cont2(x), cont)).step
      }
  }

  @scala.annotation.tailrec
  protected final def evaluate: A = step match {
    case Right(value) => value
    case Left(eval)   => eval().evaluate
  }

  final def map[B](f: A => B): Eval[B] = this match {
    case Now(a)        => Now(f(a))
    case Always(_)     => FlatMap(this, a => Now(f(a)))
    case Memoize(_)    => ???
    case FlatMap(_, _) => FlatMap(this, a => Now(f(a)))
  }

  final def flatMap[B](f: A => Eval[B]): Eval[B] = this match {
    case Now(a)        => f(a)
    case Always(_)     => FlatMap(this, a => f(a))
    case Memoize(_)    => FlatMap(this, a => f(a))
    case FlatMap(_, _) => FlatMap(this, a => f(a))
  }

  final def memoize: Eval[A] = Memoize(() => this)
}

object Eval {

  def now[A](a: => A): Eval[A]         = Now(a)
  def memoize[A](a: => A): Eval[A]     = Memoize(() => now(a))
  def always[A](a: => A): Eval[A]      = Always(() => now(a))
  def defer[A](a: => Eval[A]): Eval[A] = Always(() => a)

  val True: Eval[Boolean]  = Now(true)
  val False: Eval[Boolean] = Now(false)

  private final case class Now[A](a: A)                 extends Eval[A]
  private final case class Always[A](fn: () => Eval[A]) extends Eval[A]
  private final case class Memoize[A](f: () => Eval[A]) extends Eval[A] {
    private[this] var thunk: () => A = () => f().value

    override lazy val value: A = {
      val result = thunk()
      thunk = null
      result
    }
  }
  private final case class FlatMap[A, B](eval: Eval[A], cont: A => Eval[B]) extends Eval[B]
}
