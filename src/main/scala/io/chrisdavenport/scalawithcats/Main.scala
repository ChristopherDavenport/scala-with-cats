package io.chrisdavenport.scalawithcats

// import chapter3._
import cats._
import cats.implicits._
// import Codec._

object Main {

  def main(args: Array[String]): Unit = {
    // val trueString = "true"

    def add1[F[_]](fa: F[Int])(implicit F: Functor[F]): F[Int] = F.map(fa)(_ + 1)

    println(add1(Option(1)))
    println(add1(List(1,2,3,4,5)))
    println(add1(Either.left[String, Int]("Yellow"))) /// Either[String, ?]

    // def mapEither[E, A, B](e: Either[E, A])(f: A => B): Either[E,B] = e match {
    //   case Right(a) =>  Right(f(a))
    //   case Left(e) => Left(e) 
    // }

    



    println(add1(Either.right[String, Int](1)))
    
    
  }

}