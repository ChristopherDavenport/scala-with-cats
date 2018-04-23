package io.chrisdavenport.scalawithcats.chapter11

import cats.Monoid

trait BoundedSemiLattice[A] extends Monoid[A]{
  def combine(a1: A, a2: A): A
  def empty: A
}
object BoundedSemiLattice {
  implicit val bslInt : BoundedSemiLattice[Int] = new BoundedSemiLattice[Int]{
    def combine(a1: Int, a2: Int): Int = 
      a1 max a2

    val empty: Int = 0
  }
  implicit def bslSets[A]: BoundedSemiLattice[Set[A]] = 
    new BoundedSemiLattice[Set[A]]{
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
      val empty: Set[A] = Set.empty[A]
    }


}