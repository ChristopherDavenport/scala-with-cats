package io.chrisdavenport.scalawithcats.chapter7

import cats.Monoid

object Scaffolding {
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = 
    l.foldRight(List.empty[B])((a, lb) => f(a) ::: lb)

  def map[A, B](l: List[A])(f: A => B): List[B] = 
    l.foldRight(List.empty[B])((a, lb) => f(a) :: lb)
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    l.foldRight(List.empty[A])((a, la) => 
      if (f(a)) a :: la else la
    )

  def sum[A: Monoid](l: List[A]): A = 
    l.foldRight(Monoid[A].empty)(Monoid[A].combine)

}