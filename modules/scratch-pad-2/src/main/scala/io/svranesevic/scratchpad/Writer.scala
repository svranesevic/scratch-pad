package io.svranesevic.scratchpad

final case class Writer[L, A](private val both: (List[L], A)) extends AnyVal {

  def run(): (List[L], A) = both

  def runEval(): A      = both._2
  def runLog(): List[L] = both._1

  def map[B](f: A => B): Writer[L, B] = {
    val (l, a) = both
    new Writer[L, B]((l, f(a)))
  }

  def flatMap[B](f: A => Writer[L, B]): Writer[L, B] = {
    val (l, a)   = both
    val (ll, aa) = f(a).both
    new Writer[L, B]((l ++ ll, aa))
  }

  def tell(e: L): Writer[L, A] = {
    val (l, a) = both
    Writer(l :+ e, a)
  }
}

object Writer {
  def apply[L, A](l: List[L], a: A): Writer[L, A] = new Writer[L, A]((l, a))

  def lift[L, A](a: A): Writer[L, A] = new Writer[L, A]((Nil, a))
}

object WriterMain extends App {
  val program =
    for {
      a <- Writer.lift(42).tell(42).tell(48).tell(485)
      b <- Writer.lift(58).tell(1).tell(2).tell(3)
    } yield a + b

  val (log, out) = program.run()
  assert(out == 100)
  assert(log == List(42, 48, 485, 1, 2, 3))
  println(s"Out: $out, log: ${log.mkString("[", ",", "]")}")
}
