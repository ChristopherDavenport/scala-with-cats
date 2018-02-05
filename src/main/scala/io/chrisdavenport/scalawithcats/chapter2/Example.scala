package io.chrisdavenport.scalawithcats.chapter2

trait Semigroup[A]{
  def combine(x: A, y: A): A
}
object Semigroup {
  def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev
}
object SemigroupInstances {
  implicit val intSemigroup: Semigroup[Int] =  new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = x + y
  }
  implicit def semigroupF[A, B](implicit S: Semigroup[B]) = new Semigroup[A => B]{
    override def combine(x: A => B, y: A => B): A => B = {a => 
      S.combine(x(a), y(a))
    }
  }
}
object Example {
  import SemigroupInstances._
  val combined = Semigroup[Int => Int].combine({x: Int =>  x + 1}, {x: Int => x * 10}).apply(6)
}



