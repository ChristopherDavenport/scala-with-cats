package io.chrisdavenport.scalawithcats.chapter7

import cats.data._
import cats.implicits._

object ValidatedTraverse {
  def process(inputs: List[Int]): Validated[List[String], List[Int]] = 
    inputs.traverse{n => 
      if(n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  val output1 = process(List(2,4,6))
  // Validated[List[String], List[Int]] = Valid(List(2, 4, 6))

  val output2 = process(List(1,2,3))
  // Validated[List[String], List[Int]] = Invalid(List("1 is not even", "3 is not even"))
}