package io.chrisdavenport.scalawithcats.chapter7

import cats.implicits._

object VectorTraverse {
  val out1 = List(Vector(1,2), Vector(3,4)).sequence
  // Vector[List[Int]] = Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

  val out2 = List(Vector(1,2), Vector(3,4), Vector(5,6)).sequence
  //  Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))

}