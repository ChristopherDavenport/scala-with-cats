package io.chrisdavenport.scalawithcats.chapter3

import cats.Functor

sealed trait NodeTree[+A]
final case class Node[A](left: NodeTree[A], value: A, right: NodeTree[A]) extends NodeTree[A]
final case object NodeLeaf extends NodeTree[Nothing]

object NodeTree{

  def nodeTreeFunctor: Functor[NodeTree] = new Functor[NodeTree]{
    override def map[A,B](fa: NodeTree[A])(f: A => B): NodeTree[B] = fa match {
      case Node(l, a, r) => Node(map(l)(f), f(a), map(r)(f))
      case NodeLeaf => NodeLeaf
    }
  }

}