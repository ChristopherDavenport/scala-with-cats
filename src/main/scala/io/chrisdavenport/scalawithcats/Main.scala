package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter5._
// import cats.implicits._
// import cats._
// import Codec._
import TransformAndRollOut._
import cats.effect._

object Main {

  def main(args: Array[String]): Unit = {
    tacticalReport("Jazz", "Bumblebee").flatMap(e => IO(println(e))).unsafeRunSync

    tacticalReport("Jazz", "Dora The Explorer").flatMap(e => IO(println(e))).unsafeRunSync

    tacticalReport("Jazz", "Hot Rod").flatMap(e => IO(println(e))).unsafeRunSync

  }

}