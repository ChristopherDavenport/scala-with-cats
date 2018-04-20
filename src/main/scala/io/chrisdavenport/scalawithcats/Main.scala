package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter10._
// import cats.implicits._
// import cats._
// import cats.effect._
// import fs2._
// import scala.concurrent.ExecutionContext.Implicits.global


object Main {

  def main(args: Array[String]): Unit = {
    import AccState._
    import Acc._

    // val f = for {
    //   _ <- modify[Int](_ + 1)
    //   value <- pure(3)
    //   _ <- modify[Int](_ + value)
    //   result <- get
    // } yield result

    val out1 = coolAcc(1).value
    val out2 = coolAcc(2).value


    println(out1)
    println(out2)

  }

}