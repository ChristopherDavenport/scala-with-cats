package io.chrisdavenport.scalawithcats.chapter4

import cats.data._
import cats.implicits._

object ShowYourWork {
  // SLOWLY
  def slowly[A](a: => A) = try a finally Thread.sleep(100)
  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def futureFailure(): Unit = {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), 5.seconds)
    ()
  }

  def factorialWriter(n: Int): Writer[Vector[String], Int] = {
    val ans = slowly(if (n == 0) 1.pure[Writer[Vector[String], ?]] else factorialWriter(n - 1).map(_ * n))
    
    for {
      a <- ans
      _ <- Vector(s"fact $n $a").tell
    } yield a
  }

  def futureWriter(): Unit = {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._
    val out = Await.result(Future.sequence(Vector(
      Future(factorialWriter(3)),
      Future(factorialWriter(3))
    )), 5.seconds)
    println(out)
  }


}