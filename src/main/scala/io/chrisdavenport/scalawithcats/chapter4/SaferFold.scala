package io.chrisdavenport.scalawithcats.chapter4

import cats._

object SaferFold {
  def foldRightE[A, B](l: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    l match {
      case head :: tail => 
        Eval.defer(f(head, foldRightE(tail, acc)(f)))
      case Nil => 
        acc
    }

    def foldRight[A, B](l: List[A], acc: B)(f: (A, B) => B): B =
      foldRightE[A, B](l, Eval.now(acc)){case (a, be) => be.map(b => f(a, b))}.value

}