package io.chrisdavenport.scalawithcats.chapter5

import cats._
// import cats.data._
import cats.implicits._

object Transformed {

  def traverse[F[_]: Traverse, G[_]: Applicative, A](f: F[G[A]]): G[F[A]]  = f.sequence



}