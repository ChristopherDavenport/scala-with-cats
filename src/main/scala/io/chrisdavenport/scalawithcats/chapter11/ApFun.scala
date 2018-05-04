package io.chrisdavenport.scalawithcats.chapter11

object ApFun {
  object Ap1 {
    import cats.Applicative

    // val listApplicative: Applicative[List] = new Applicative[List]{
    //   def pure[A](a: A): List[A] = ???

    //   def ap[A, B](ff: List[A => B])(a: List[A]): List[B] = ???
    // }

    val optionApplicative: Applicative[Option] = new Applicative[Option]{
      def pure[A](a: A): Option[A] = Option(a)

      def ap[A, B](ff: Option[A => B])(a: Option[A]): Option[B] = 
        for {
          f <- ff
          sa <- a
        } yield f(sa)
      // (ff, a) match {
      //   case (None, None) => None
      //   case (None, Some(_)) => None
      //   case (Some(_), None) => None
      //   case (Some(f), Some(a)) => pure(f(a))
      // }
    }

    def map[F[_]: Applicative, A, B](fa: F[A])(f: A => B): F[B] = {
      Applicative[F].ap(Applicative[F].pure(f))(fa)
    }

    def product[F[_]: Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      Applicative[F].ap(
        Applicative[F].map(fa)(a => (b: B) => (a, b)) // F[B => (A, B)]
      )(
        fb
      )
    }

    import cats.data.Validated
    import cats.data.Validated.{Invalid, Valid}
    import cats.Semigroup

    def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] = new Applicative[Validated[E, ?]]{
      def pure[A](a: A): Validated[E, A] = Valid(a)
      def ap[A, B](ff: Validated[E, A => B])(a: Validated[E, A]): Validated[E, B] = 
        (a, ff) match {
          case (Valid(a), Valid(f)) => Valid(f(a))
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
          case (e@Invalid(_), _) => e
          case (_ , e@Invalid(_)) => e
        }
    }
  }

  object Ap2 {
    import cats._ 
    // import cats.implicits._

    trait Op1[F[_]]{
      def cool: F[Int]
    }

    trait Op2[F[_]]{
      def stringCool: F[String]
    }

    def comp[F[_]: Applicative](o1: Op1[F], o2: Op2[F]): F[(Int, String)] = {
      Applicative[F].product(o1.cool, o2.stringCool)
    }
  }


}