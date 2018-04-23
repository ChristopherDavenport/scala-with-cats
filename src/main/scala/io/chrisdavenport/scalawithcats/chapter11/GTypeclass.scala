package io.chrisdavenport.scalawithcats.chapter11

import cats._
import cats.implicits._

trait GTypeclass[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)
        (implicit m: Monoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])
        (implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])
        (implicit m: Monoid[V]): V
}
object GTypeclass {
  def apply[F[_,_], K, V]
        (implicit counter: GTypeclass[F, K, V]) =
    counter
  
  object Instances {
    implicit def mapInst[K, V]: GTypeclass[Map, K, V] = new GTypeclass[Map, K, V]{
      def increment(f: Map[K, V])(k: K, v: V)
        (implicit m: Monoid[V]): Map[K, V] = f + (k -> (v |+| f.getOrElse(k, m.empty)))

      def merge(f1: Map[K, V], f2: Map[K, V])
        (implicit b: BoundedSemiLattice[V]): Map[K, V] = 
        f1 |+| f2

      def total(f: Map[K, V])
        (implicit m: Monoid[V]): V = f.values.toList.combineAll
    }

    def testMapInst(): Unit = {
      val g1 = Map("a"-> 7, "b"-> 3)
      println(g1)
      val g2 = Map("a" -> 2, "b" -> 5)
      println(g2)
      val counter = GTypeclass[Map, String, Int]
      val merged = counter.merge(g1, g2)
      println(merged)
      val total = counter.total(merged)
      println(total)
    }

    implicit def gcounterInstance[F[_,_], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) = {
      import KeyValueStore.Ops._
      new GTypeclass[F, K, V] {
        def increment(f: F[K, V])(key: K, value: V)(implicit m: Monoid[V]): F[K, V] = {
          val total = f.getOrElse(key, m.empty) |+| value
          f.put(key, total)
        }
        def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
          f1 |+| f2
        def total(f: F[K, V])(implicit m: Monoid[V]): V =
          f.values.combineAll
      }
    }
  }
}