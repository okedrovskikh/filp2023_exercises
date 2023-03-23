package exercises04

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    @tailrec
    def foldTailrec[A, B](t: Tree[A], acc: List[Tree[A]])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) =>
        acc match {
          case Nil          => f(v)
          case head :: tail => foldAcc(f(v), head :: tail)(f)(g)
        }
      case Branch(left, right) => foldTailrec(left, right :: acc)(f)(g)
    }

    @tailrec
    def foldAcc[A, B](left: B, acc: List[Tree[A]])(f: A => B)(g: (B, B) => B): B = acc match {
      case head :: tail => foldAcc(g(left, foldTailrec(head, List())(f)(g)), tail)(f)(g);
      case Nil          => left
    }

    foldTailrec(t, List())(f)(g)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => x + y + 1)

  def max(t: Tree[Int]): Int = fold(t)(t => t)((l, r) => scala.math.max(l, r))

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => scala.math.max(l, r) + 1)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(x => Leaf(f(x)))((l, r) => Branch(l, r))
}
