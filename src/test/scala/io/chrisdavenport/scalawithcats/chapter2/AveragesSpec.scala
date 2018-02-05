package io.chrisdavenport.scalawithcats.chapter2

import org.scalacheck.Arbitrary
// import cats.tests.CatsSuite
// import cats.laws._
import cats.kernel.laws.discipline._
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline
import cats.implicits._

object AveragesSpec extends FunSuite with Matchers with Discipline {
  implicit val arbAverages: Arbitrary[Averages] = Arbitrary{
    for {
      double <- Arbitrary.arbitrary[Double]
      count <- Arbitrary.arbitrary[Int]
    } yield {
      if (count == 0) Averages(0D, 0)
      else  Averages(double, count)
    }
  }

  checkAll("Eq[List[HasEq[Int]]]", EqTests[List[Int]].eqv)
  checkAll("Averages Semigroup", SemigroupTests[Averages].semigroup)
  checkAll("Averages Semigroup", SemigroupTests[Int].semigroup)
}