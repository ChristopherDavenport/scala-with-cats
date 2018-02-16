package io.chrisdavenport.scalawithcats.chapter4

import cats.Monad

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad : Monad[Tree] = new Monad[Tree]{
    override def pure[A](a: A) = leaf(a)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(l,r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???
  }

}