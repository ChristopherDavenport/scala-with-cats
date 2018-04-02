package io.chrisdavenport.scalawithcats.chapter7

import cats._
import cats.implicits._

object OptionTraverse {
  // def process(inputs: List[Int]): Option[List[Int]] =
  //   inputs.traverse(n => if(n % 2 == 0) Some(n) else None)

  // val out1 = process(List(2,4,6))
  // // Option[List[Int]] = Some(List(2, 4, 6))

  // val out2 = process(List(1,2,3))
  // // Option[List[Int]] = None

  // import cats.Traverse
  // import cats.Applicative

  // def sequence[F[_]: Traverse, G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = fga.sequence


    def optionTraverse[A, B](fa: Option[A])(f: A => List[B]): List[Option[B]] = fa match {
      case None => Applicative[List].pure(Option.empty[B])
      case Some(a) => Functor[List].map(f(a))(Applicative[Option].pure)
    }

    def listTraverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      def go(rest: List[A], acc: G[List[B]]): G[List[B]] = (rest, acc) match {
        case (Nil, acc) => acc
        case (x :: xs, ol) => go(xs, Apply[G].map2(ol, f(x))((l,o) => o :: l))
      }

      go(fa, Applicative[G].pure(List.empty[B]))
    }

    def traverseOption[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      fa match {
        case None => Applicative[G].pure(Option.empty[B])
        case Some(a) => f(a).map(Applicative[Option].pure)
      }
    }

}