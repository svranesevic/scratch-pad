package io.svranesevic.scratchpad

import scala.annotation.tailrec

sealed trait LazyList[+A] {

  import LazyList._

  def toList: List[A] =
    this match {
      case Cons(hd, tl) => hd() :: tl().toList
      case Empty        => Nil
    }

  // def headOption: Option[A] =
  //   this match {
  //     case Cons(hd, tl) => Some(hd())
  //     case Empty        => None
  //   }

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))

  // def take(n: Long): ZTream[A] =
  //   this match {
  //     case Cons(hd, tl) if n > 0 => cons(hd(), tl().take(n - 1))
  //     case _                     => empty
  //   }

  def take(n: Long): LazyList[A] =
    unfold((this, n)) {
      case (Cons(hd, tl), n) if n > 0 => Some((hd(), (tl(), n - 1)))
      case _                          => None
    }

  // def takeWhile(f: A => Boolean): ZTream[A] =
  //   this match {
  //     case Cons(hd, tl) if f(hd()) => cons(hd(), tl().takeWhile(f))
  //     case _                       => Empty
  //   }

  // def takeWhile(f: A => Boolean): ZTream[A] =
  //   foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def takeWhile(f: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(hd, tl) if f(hd()) => Some(hd(), tl())
      case _                       => None
    }

  def drop(n: Long): LazyList[A] =
    this match {
      case Cons(hd, tl) if n > 0 => tl().drop(n - 1)
      case other                 => other
    }

  // def exists(f: A => Boolean): Boolean =
  //   this match {
  //     case Cons(hd, tl) => f(hd()) || tl().exists(f)
  //     case Empty        => false
  //   }

  def exists(f: A => Boolean): Boolean =
    foldRight(false)((a, b) => f(a) || b)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  // def map[B](f: A => B): ZTream[B] =
  //   foldRight(empty[B])((a, b) => cons(f(a), b))

  def map[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty      => None
    }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that)((a, b) => cons(a, b))

  def append[B >: A](that: B): LazyList[B] =
    append(LazyList(that))

  def zip[B](that: LazyList[B]): LazyList[(A, B)] =
    unfold((this, that)) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) =>
        Some((hd1(), hd2()), (tl1(), tl2()))

      case _ => None
    }

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) =>
        Some(f(hd1(), hd2()), (tl1(), tl2()))

      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(hd1, tl1), Cons(hd2, tl2)) =>
        val a = Some(hd1())
        val b = Some(hd2())
        Some((a, b), (tl1(), tl2()))

      case (Cons(hd, tl), Empty) =>
        Some(((Some(hd()), None), (tl(), Empty)))

      case (Empty, Cons(hd, tl)) =>
        Some(((None, Some(hd())), (Empty, tl())))

      case (Empty, Empty) => None
    }

  def isEmpty: Boolean =
    this match {
      case Empty => true
      case _     => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
      case _            => z
    }

  def startsWith[B >: A](that: LazyList[B]): Boolean =
    this.zipWith(that)(_ == _).forAll(_ == true)

  def hasSubsequence[B >: A](that: LazyList[B]): Boolean =
    tails.exists(_.startsWith(that))

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case s @ Cons(_, tl) => Some(s, tl())
      case _               => None
    }

}
object LazyList {
  private case object Empty extends LazyList[Nothing]
  private case class Cons[+A](head: () => A, tail: () => LazyList[A]) extends LazyList[A]

  def constant[A](a: A): LazyList[A] = unfold(a)(_ => Some(a -> a))

  def from(n: Int): LazyList[Int] = unfold(n)(s => Some((s, s + 1)))

  val fibonacci: LazyList[Int] =
    LazyList.unfold((0, 1)) { case (a, b) =>
      Some((a, (b, a + b)))
    }

  def apply[A](as: A*): LazyList[A] =
    if (as.isEmpty) empty
    else cons(as.head, LazyList(as.tail: _*))

  def empty[A]: LazyList[A] = Empty

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(s) match {
      case Some((value, state)) =>
        LazyList.cons(value, unfold(state)(f))
      case None => LazyList.empty
    }

  private def cons[A](head: => A, tail: => LazyList[A]): LazyList[A] = {
    // Ensures head and tail are evaluated only once - memoized
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)
  }
}

object LazyListMain extends App {
  val stream = LazyList(1, 2, 3, 4, 5)

  assert(stream.toList == List(1, 2, 3, 4, 5))

  assert(stream.headOption == Some(1))
  assert(LazyList.empty.headOption == None)

  assert(stream.take(2).toList == List(1, 2))

  assert(stream.drop(2).toList == List(3, 4, 5))

  assert(stream.takeWhile(_ < 4).toList == List(1, 2, 3))

  assert(stream.exists(_ == 5) == true)
  assert(stream.exists(_ == 42) == false)

  assert(stream.forAll(_ != 0) == true)
  assert(stream.forAll(_ != 1) == false)

  assert(stream.map(_ * 2).toList == List(2, 4, 6, 8, 10))

  assert(stream.flatMap(i => LazyList(i * 2)).toList == List(2, 4, 6, 8, 10))

  assert(stream.filter(_ % 2 == 0).toList == List(2, 4))

  assert(stream.append(6).append(7).toList == List(1, 2, 3, 4, 5, 6, 7))
  assert(stream.append(LazyList(6, 7)).toList == List(1, 2, 3, 4, 5, 6, 7))

  assert(stream.find(_ == 4) == Some(4))
  assert(stream.find(_ == 42) == None)

  assert(LazyList.constant(1).take(100).toList == List.fill(100)(1))
  assert(LazyList.from(10).take(5).toList == List(10, 11, 12, 13, 14))
  assert(LazyList.fibonacci.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))

  val streamZipAll = LazyList(6, 7, 8, 9)
  val allZipped =
    List(
      (Some(1), Some(6)),
      (Some(2), Some(7)),
      (Some(3), Some(8)),
      (Some(4), Some(9)),
      (Some(5), None)
    )
  assert(stream.zipAll(streamZipAll).toList == allZipped)

  val streamZip = LazyList(6, 7, 8, 9)
  val zipped =
    List(
      (1, 6),
      (2, 7),
      (3, 8),
      (4, 9)
    )
  assert(stream.zip(streamZip).toList == zipped)

  assert(stream.zipWith(LazyList(4, 3, 2))(_ + _).toList == List(5, 5, 5))

  assert(stream.startsWith(LazyList(1, 2, 3)) == true)
  assert(stream.startsWith(LazyList(1, 2, 3, 4, 5)) == true)
  assert(stream.startsWith(LazyList(1, 42)) == false)

  val streamTails =
    List(
      List(1, 2, 3, 4, 5),
      List(2, 3, 4, 5),
      List(3, 4, 5),
      List(4, 5),
      List(5)
    )
  assert(stream.tails.toList.map(_.toList) == streamTails)

  assert(stream.hasSubsequence(LazyList(2, 3, 4)))

  // Doesn't blow up stack
  val ones = LazyList.constant(1)
  ones.map(_ + 1).exists(_ % 2 == 0)
  ones.takeWhile(_ == 1)
  ones.forAll(_ != 1)
}
