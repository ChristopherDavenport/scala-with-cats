package io.chrisdavenport.scalawithcats.chapter5

import cats._
import cats.data._
// import cats.implicits._


object CompositionFatigue {

  // stack = F[Either[String, Option[Writer[Vector[String], A]]]]
  // result = F[Option[Writer[Vector[String], Either[String, A]]]]

  def stackToStack[F[_]: Monad, A](stack: WriterT[OptionT[EitherT[F, String, ?], ?], Vector[String], A]): 
    EitherT[WriterT[OptionT[F, ?], Vector[String], ?], String, A] = {
      val _ = stack
      ???
    }
    


}