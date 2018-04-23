package io.chrisdavenport.scalawithcats.chapter11

import cats._ 
import cats.implicits._

final case class GenGCounter[K, A](counters: Map[K, A]){
  def increment(k: K, a: A)(implicit M: Monoid[A]) = GenGCounter(counters + (k -> (a |+| counters.getOrElse(k, M.empty))))
  def merge(that: GenGCounter[K, A])(implicit BSL: BoundedSemiLattice[A]): GenGCounter[K, A] = 
    GenGCounter(this.counters |+| that.counters)
  def total(implicit M: Monoid[A]): A = counters.values.toList.combineAll
}