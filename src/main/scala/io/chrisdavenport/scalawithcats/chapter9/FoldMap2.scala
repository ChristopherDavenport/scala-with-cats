package io.chrisdavenport.scalawithcats.chapter9

import cats._
import cats.implicits._
import cats.effect._
import scala.concurrent.ExecutionContext

object FoldMap2 {
  def foldMap[A, B: Monoid](v: Vector[A])(f: A => B): B = {
    v.map(f).foldRight(Monoid[B].empty){ case (next, acc) =>
      Monoid[B].combine(next, acc)
    }
  }

  def foldMapGen[F[_]: Foldable: Functor, A, B: Monoid](fa: F[A])(f: A => B): B = {
    val map :  (A => B) => F[A] => F[B] = localF => localFa => Functor[F].map(localFa)(localF)
    val reduce: F[B] => B = localFb => 
    Foldable[F].foldRight(localFb, Eval.now(Monoid[B].empty)){ case (next, evalB) =>
      Eval.defer{
        evalB.map(bOnRight => Monoid[B].combine(next, bOnRight))
      }
    }.value

    (map(f) andThen reduce)(fa)
  }

  def parFoldMap[A, B: Monoid](v: List[A], concurrency: Int)
                              (f: A => B)
                              (implicit ec: ExecutionContext): IO[B] = 
                              IO.suspend{
                            
    v.grouped(v.size / concurrency)
      .toList
      .traverse(
        v => IO.shift.flatMap(_ => 
          IO(foldMapGen(v)(f))
        )
      ).map(_.foldRight(Monoid[B].empty){ case (next, acc) =>
          Monoid[B].combine(next, acc)
      })
  }

  def parFoldMap[F[_]: Foldable: Functor, G[_]: Async, A, B: Monoid](fa: F[A], concurrency: Int)
                (f: A => B)(implicit ec: ExecutionContext): G[B] = 
                Sync[G].suspend{
      val initList = fa.toList
      val size = fa.size
      val groupSize = (size / concurrency).toInt
      initList
      .grouped(groupSize)
      .toList
      .foldMapM{listA => 
        Async.shift[G](ec).flatMap(_ => 
            Sync[G].delay(listA.foldMap(f))
        )
      }
    }

}