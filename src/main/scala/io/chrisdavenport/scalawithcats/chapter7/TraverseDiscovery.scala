package io.chrisdavenport.scalawithcats.chapter7

import cats.Applicative
import cats.implicits._
import scala.annotation.tailrec

object TraverseDiscovery {

  // trait Traverse[F[_]]{
  //   def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  // }
  def myTraverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {

    @tailrec
    def traverseMe(fa: List[A], acc: G[List[B]])(f: A => G[B]): G[List[B]] = (fa, acc) match {
      case (Nil, acc) => acc.map(_.reverse)
      case (x :: xs, ol) => traverseMe(
        xs, 
        Applicative[G].map2(ol,f(x))((listB, b) => b :: listB)
      )(f)
    }
    traverseMe(fa, Applicative[G].pure(List.empty[B]))(f)
  }

}