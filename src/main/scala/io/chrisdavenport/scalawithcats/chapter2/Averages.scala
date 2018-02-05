package io.chrisdavenport.scalawithcats.chapter2

import cats._
import cats.implicits._

final case class Averages(currentAverage: Double, numberOfElements: Int)

object Averages {

  def currentAverage(a: Averages): Averages = a

  implicit val averageSemigroup : Semigroup[Averages] = new Semigroup[Averages]{
    override def combine(x: Averages, y: Averages): Averages = {
      val newSum = x.currentAverage * x.numberOfElements +  y.currentAverage * y.numberOfElements
      val newCount = x.numberOfElements + y.numberOfElements
      val newAverage = if (newCount == 0) 0D else newSum / newCount
      Averages(newAverage, newCount)
    }
  }

  implicit val averagesEq : Eq[Averages] = new Eq[Averages] {
    override def eqv(x: Averages, y: Averages): Boolean = 
      x.currentAverage === y.currentAverage && x.numberOfElements === y.numberOfElements
  }

}