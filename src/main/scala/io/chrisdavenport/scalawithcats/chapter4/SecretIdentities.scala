package io.chrisdavenport.scalawithcats.chapter4

object SecretIdentities {
    type Id[A] = A

    trait Monad[F[_]]{
        def pure[A](a: A): F[A]
        def flatMap[A,B](fa: F[A])(f: A => F[B]): B
    }

    object MonadInstances {

        implicit val idMonad : Monad[Id] = new Monad[Id]{
            override def pure[A](a: A): Id[A] = a
            override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
        }

    }


}