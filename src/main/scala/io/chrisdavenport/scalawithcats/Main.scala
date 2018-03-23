package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter7._
import cats.implicits._
// import cats._
import cats.effect._

object Main {

  def main(args: Array[String]): Unit = {

    val traversed = TraverseDiscovery.myTraverse[IO, Int, Int](List(1,2,3)){ int => 
      if (int % 2 == 0) IO.raiseError[Int](new Throwable("Boom!")) else IO(println(int)).as(int)
    }
    traversed.void.unsafeRunSync
  }

}