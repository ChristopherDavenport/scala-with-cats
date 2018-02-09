package io.chrisdavenport.scalawithcats.chapter3

import cats._

trait FunctionMine[A, B]{
  def apply(a: A): B
}

object FunctionMine {

    val functionIntString = new FunctionMine[String, Int]{
      def apply(a: String): Int = a.length
    }

    // val increaseByOne : FunctionMine[String, Double] = functionIntString.map(_.toDouble)
  
    implicit def functorFunctionMine[E]: Functor[FunctionMine[E, ?]] = new Functor[FunctionMine[E, ?]]{
      override def map[A, B](fa: FunctionMine[E, A])(f: A => B): FunctionMine[E, B] = new FunctionMine[E, B]{
        def apply(a: E): B = f(fa.apply(a))
      }
    }

    implicit def contravariantFunctionMine[E]: Contravariant[FunctionMine[?, E]] = new Contravariant[FunctionMine[?, E]]{
      override def contramap[A,B](fa: FunctionMine[A, E])(f: B => A): FunctionMine[B, E] = new FunctionMine[B, E]{
        def apply(a: B): E = fa.apply(f(a))
      }
    }

}