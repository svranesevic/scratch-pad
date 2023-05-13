package io.svranesevic.scratchpad

// For Scala 3 variant based on match types see Zipped.scala
trait Zippable[-A, -B] {

  type Out

  def zip(a: A, b: B): Out
}

object Zippable extends ZippableLowPriority1 {

  type Out[-A, -B, Out0] = Zippable[A, B] { type Out = Out0 }

  implicit def leftIdentity[B]: Zippable.Out[Unit, B, B] =
    new Zippable[Unit, B] {
      type Out = B
      override def zip(a: Unit, b: B): B = b
    }
}

trait ZippableLowPriority1 extends ZippableLowPriority2 {

  implicit def rightIdentity[A]: Zippable.Out[A, Unit, A] =
    new Zippable[A, Unit] {
      type Out = A
      override def zip(a: A, b: Unit): A = a
    }
}

trait ZippableLowPriority2 extends ZippableLowPriority3 {

  // This is the point where Scala 3 Match Types wins, see Zipped.scala
  implicit def either[L, R1, R2](implicit
    zippable: Zippable[R1, R2]
  ): Zippable.Out[Either[L, R1], Either[L, R2], Either[L, zippable.Out]] =
    new Zippable[Either[L, R1], Either[L, R2]] {
      type Out = Either[L, zippable.Out]
      override def zip(a: Either[L, R1], b: Either[L, R2]): Either[L, zippable.Out] =
        a.flatMap(a => b.map(b => zippable.zip(a, b)))
    }

  implicit def tuple3[A, B, C]: Zippable.Out[(A, B), C, (A, B, C)] =
    new Zippable[(A, B), C] {
      type Out = (A, B, C)
      override def zip(ab: (A, B), c: C): (A, B, C) = (ab._1, ab._2, c)
    }

  implicit def tuple4[A, B, C, D]: Zippable.Out[(A, B, C), D, (A, B, C, D)] =
    new Zippable[(A, B, C), D] {
      type Out = (A, B, C, D)
      override def zip(abc: (A, B, C), d: D): (A, B, C, D) = (abc._1, abc._2, abc._3, d)
    }

  implicit def tuple5[A, B, C, D, E]: Zippable.Out[(A, B, C, D), E, (A, B, C, D, E)] =
    new Zippable[(A, B, C, D), E] {
      type Out = (A, B, C, D, E)
      override def zip(abcd: (A, B, C, D), e: E): (A, B, C, D, E) = (abcd._1, abcd._2, abcd._3, abcd._4, e)
    }

  // etc.
}

trait ZippableLowPriority3 {

  implicit def tuple[A, B]: Zippable.Out[A, B, (A, B)] =
    new Zippable[A, B] {
      type Out = (A, B)
      override def zip(a: A, b: B): (A, B) = (a, b)
    }
}
