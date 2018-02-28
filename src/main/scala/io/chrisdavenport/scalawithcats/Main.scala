package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter4._
import cats.implicits._
import cats._
// import Codec._

object Main {

  def main(args: Array[String]): Unit = {
    val composed = Tree.leaf(1) <+> Tree.leaf(2)
    println(composed)

    composed.traverse(i => Eval.now(println(show"Found $i"))).void.value
    composed
      .flatMap(i => Tree.branch(Tree.leaf(i - 1), Tree.leaf(i + 1)))
      .pure[Eval]
      .map(println(_))
      .value

  }

}