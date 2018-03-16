package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter6._
import cats.implicits._
// import cats._
// import Codec._
import cats.effect._

object Main {

  def main(args: Array[String]): Unit = {
    LoggingInPureFunctions.withPureTransformations(
      List(
        1 -> "Yellow",
        3 -> "Monkeys",
        0 -> "Oh Noes!"
      )
    ).flatMap{ listOfOptions => 
      IO(println(s"Result Was: $listOfOptions"))
    }.handleErrorWith(e => IO(println("Got Error")))
    .unsafeRunSync
  }

}