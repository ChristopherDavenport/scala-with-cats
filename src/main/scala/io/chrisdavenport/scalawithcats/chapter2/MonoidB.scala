package io.chrisdavenport.scalawithcats.chapter2

trait SemigroupB[A]{
  def combine(a1: A, a2: A): A
}

trait MonoidB[A]{
  def semigroup: SemigroupB[A]
  def zero: A
}

object MonoidB {
  def apply[A](implicit ev: MonoidB[A]): MonoidB[A] = ev
}

object MonoidBInstances{

}

object CatsMonoid {
  // SuperAdder v3.5a-32
  import cats._
  def add[A: Monoid](items: List[A]): A = {
    val M: Monoid[A] = Monoid[A]
    items.foldRight(M.empty)(M.combine)
  }

  final case class Order(totalCost: Double, quantity: Double)

  object Order {
    implicit val orderMonoid: Monoid[Order] = new Monoid[Order]{
      override def empty = Order(0, 0)
      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }
  }

}