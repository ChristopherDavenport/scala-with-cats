package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter4._
import cats.implicits._
// import cats._
// import cats.implicits._
// import Codec._


object Main {

  def main(args: Array[String]): Unit = {
    val t = Tree.fill(10000)(1)
      .flatMap(i => Branch(i.pure[Tree], Branch((i - 1).pure[Tree], (i + 1).pure[Tree])))
    val _ = t

  }

}