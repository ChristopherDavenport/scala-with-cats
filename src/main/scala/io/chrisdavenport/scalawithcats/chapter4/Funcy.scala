package io.chrisdavenport.scalawithcats.chapter4

object Funcy {
    trait Monad[F[_]]{
        def pure[A](a: A): F[A]
        def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
        def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
    }

    trait Monad2[F[_]]{
        def join[A](fa: F[F[A]]): F[A]
        def map[A, B](fa: F[A])(f1: A => B): F[B]

        def flatMap[A, B](fa: F[A])(f2: A => F[B]): F[B] = {
            val ffb: F[F[B]] = map(fa)(f2)
            join(ffb)
        }
    }

    object Monad2Instances {
        val Monad2Option : Monad2[Option] = new Monad2[Option]{

            override def join[A](fa: Option[Option[A]]): Option[A] = fa match {
                case Some(Some(a)) => Some(a)
                case Some(None) => None
                case None => None 
            }

            override def map[A, B](fa: Option[A])(f1: A => B): Option[B] = fa.map(f1)
        }

    }

}