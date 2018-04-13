package io.chrisdavenport.scalawithcats

// import chapter3._
import chapter10._
import cats.implicits._
// import cats._
// import cats.effect._
// import fs2._
// import scala.concurrent.ExecutionContext.Implicits.global


object Main {

  def main(args: Array[String]): Unit = {
    val a = DataValidation.Predicate.lift[List[String], Int](int => 
      if (int > 2) cats.data.Validated.Valid(int)
      else cats.data.Validated.Invalid(List("Int Not > 2"))
    )
    val b = DataValidation.Predicate.lift[List[String], Int](int => 
      if (int > -2) cats.data.Validated.Valid(int)
      else cats.data.Validated.Invalid(List("Int Not > -2"))
    )
    val aCheck = DataValidation.Check.lift(a)
    // val bCheck = DataValidation.Check.lift(b)
    val out1 = aCheck.map(_.toString + "!")(4)
    val out2 = DataValidation.Check.lift(a and b)(-5)
    val out3 = DataValidation.Check.lift(a)
      .flatMap(_ => DataValidation.Check.lift(b))(-5)
    println(out1)
    println(out2)
    println(out3)

  }

}