package io.svranesevic.scratchpad

import scala.language.implicitConversions

// Naïve approach works for a simple ADT
object Simple {

  import Tree._

  sealed trait Tree[+A] {

    // To relieve caller from burden of recursion, as arguments we accept functions that
    // take care of transforming every ADT leaf type into the resulting summary type `S`.
    // This starts being problematic when number of ADT leaf types grows enough, eg. Think language AST representation
    def fold[S](leaf: A => S)(branch: (S, S) => S): S =
      this match {
        case Leaf(value) => leaf(value)
        case Branch(left, right) =>
          branch(left.fold(leaf)(branch), right.fold(leaf)(branch))
      }

    def numLeaves(): Int = {
      // Curried fold signature helps type inference
      // Otherwise we'd have to go with fold[Int](_ => 1, _ + _)
      fold(_ => 1)(_ + _)
    }

    def sumElements[B >: A]()(implicit ev: Numeric[B]): Double =
      fold[Double](ev.toDouble)(_ + _)

    def oddElements[B >: A]()(implicit ev: Numeric[B]): List[A] =
      fold[List[A]] {
        case value if ev.toInt(value) % 2 != 0 => value :: Nil
        case _                                 => Nil
      }(_ ++: _)

    def evenElements[B >: A]()(implicit ev: Numeric[B]): List[A] =
      fold[List[A]] {
        case value if ev.toInt(value) % 2 == 0 => value :: Nil
        case _                                 => Nil
      }(_ ++: _)
  }

  object Tree {
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  }
}

// 💩 getting serious aka Imagine we have language AST instead of the Tree 👇
object Advanced {

  import Tree._
  import Tree.TreeCase

  final case class Tree[+A](caseValue: TreeCase[A, Tree[A]]) {

    //      def fold[B](f: TreeCase[A, B] => B): B =
    //        caseValue match {
    //          case LeafCase(value)         => f(LeafCase(value))
    //          case BranchCase(left, right) => f(BranchCase(left.fold(f), right.fold(f)))
    //        }

    def fold[S](f: TreeCase[A, S] => S): S =
      f(caseValue.map(_.fold(f)))

    def transform[A1](f: TreeCase[A, Tree[A1]] => TreeCase[A1, Tree[A1]]): Tree[A1] =
      caseValue match {
        case LeafCase(value)         => Tree(f(LeafCase(value)))
        case BranchCase(left, right) => Tree(f(BranchCase(left.transform(f), right.transform(f))))
      }

    def numLeaves(): Int =
      fold[Int] {
        case LeafCase(_)             => 1
        case BranchCase(left, right) => left + right
      }

    def sumElements[B >: A]()(implicit ev: Numeric[B]): Double =
      fold[Double] {
        case LeafCase(value)         => ev.toDouble(value)
        case BranchCase(left, right) => left + right
      }

    def oddElements[B >: A]()(implicit ev: Numeric[B]): List[A] =
      fold[List[A]] {
        case LeafCase(value) if ev.toInt(value) % 2 != 0 => value :: Nil
        case LeafCase(_)                                 => Nil
        case BranchCase(left, right)                     => left ++: right
      }

    def evenElements[B >: A]()(implicit ev: Numeric[B]): List[A] =
      fold[List[A]] {
        case LeafCase(value) if ev.toInt(value) % 2 == 0 => value :: Nil
        case LeafCase(_)                                 => Nil
        case BranchCase(left, right)                     => left ++: right
      }
  }

  object Tree {
    sealed trait TreeCase[+E, +A] {
      def map[B](f: A => B): TreeCase[E, B] =
        this match {
          case LeafCase(value)         => LeafCase(value)
          case BranchCase(left, right) => BranchCase(f(left), f(right))
        }
    }

    case class LeafCase[E](value: E) extends TreeCase[E, Nothing]
    case class BranchCase[Self](left: Self, right: Self) extends TreeCase[Nothing, Self]

    def leaf[E](value: E): Tree[E] = Tree(LeafCase(value))
    def branch[E](left: Tree[E], right: Tree[E]): Tree[E] = Tree(BranchCase(left, right))
  }
}

object RecursionSchemesMain extends App {

  val tree = {
    import Simple.Tree._
    Branch(
      Branch(
        Leaf(42),
        Leaf(42)
      ),
      Branch(
        Leaf(109),
        Leaf(47)
      )
    )
  }
  println("Simple:")
  println(s"  Num leaves: ${tree.numLeaves()}")
  println(s"  Sum elements: ${tree.sumElements()}")
  println(s"  Odd elements: ${tree.oddElements()}")
  println(s"  Even elements: ${tree.evenElements()}")

  val recursiveTree = {
    import Advanced.Tree._
    branch(
      branch(
        leaf(42),
        leaf(42)
      ),
      branch(
        leaf(109),
        leaf(47)
      )
    )
  }
  println("Recursion Schemes:")
  println(s"  Num leaves: ${recursiveTree.numLeaves()}")
  println(s"  Sum elements: ${recursiveTree.sumElements()}")
  println(s"  Odd elements: ${recursiveTree.oddElements()}")
  println(s"  Even elements: ${recursiveTree.evenElements()}")

  val transformedTree = {
    import Advanced.Tree._
    recursiveTree.transform[Int] {
      case LeafCase(value) => LeafCase(value * 2)
      case other           => other
    }
  }
  println("Recursion Schemes with transformation := value = value * 2:")
  println(s"  Num leaves: ${transformedTree.numLeaves()}")
  println(s"  Sum elements: ${transformedTree.sumElements()}")
  println(s"  Odd elements: ${transformedTree.oddElements()}")
  println(s"  Even elements: ${transformedTree.evenElements()}")
  println(s"  Before transformation: $recursiveTree")
  println(s"  After transformation: $transformedTree")
}
