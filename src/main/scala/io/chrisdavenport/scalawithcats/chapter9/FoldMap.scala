package io.chrisdavenport.scalawithcats.chapter9

import cats._
// import cats.implicits._
// import cats.effect._

object FoldMap {
  def foldMap[G[_]: Foldable : Functor, A, B: Monoid](v: G[A])(f: A => B): B = {
    val map : G[A] => G[B] = g => Functor[G].map(g)(f)
    val reduce : G[B] => B = g => Foldable[G].foldRight[B, B](g, Eval.now(Monoid[B].empty)){ case (g, evalB) => 
      Eval.defer(
        evalB.map(lastB => 
          Monoid[B].combine(g, lastB)
        )
      )
    }.value

    (reduce compose map)(v)
  }

  // def parrallelFoldMap[A, B: Monoid](values: Vector[A], parrallelism: Int)(f: A => B): IO[B] = {
  //   values.grouped(values.length / parrallelism).toVector.traverse(l => IO(foldMap(l)(f)))
  // }
}