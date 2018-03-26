package io.chrisdavenport.scalawithcats.chapter7

import cats.implicits._

object OptionTraverse {
  def process(inputs: List[Int]): Option[List[Int]] =
    inputs.traverse(n => if(n % 2 == 0) Some(n) else None)


  val out1 = process(List(2,4,6))
  // Option[List[Int]] = Some(List(2, 4, 6))

  val out2 = process(List(1,2,3))
  // Option[List[Int]] = None


}