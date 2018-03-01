package io.chrisdavenport.scalawithcats.chapter6

import cats.Monad

object MonadProduct {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = 
    Monad[M].flatMap(x){a => Monad[M].map(y){b => (a, b)}}

  def productWithSyntax[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    for {
      a <- x
      b <- y
    } yield (a, b)
  }


}