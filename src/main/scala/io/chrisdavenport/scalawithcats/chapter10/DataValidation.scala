package io.chrisdavenport.scalawithcats.chapter10

import cats._
import cats.data._
import cats.implicits._

object DataValidation {
  
// • A user must be over 18 years old or must have parental consent.
// • A String ID must be parsable as a Int and the Int must correspond to a valid record ID.
// • A bid in an auction must apply to one or more items and have a positive value.
// • A username must contain at least four characters and all characters must be alphanumeric.


  sealed trait Predicate[E, A]{
    import Predicate._
    def and(that: Predicate[E, A]): Predicate[E, A] = {
      And(this, that)
    }
    def or(that: Predicate[E, A]): Predicate[E, A] = {
      Or(this, that)
    }

    def apply(a: A)(implicit S: Semigroup[E]): Validated[E, A] = this match {
      case Pure(f) => f(a)
      case And(l, r) => (l(a), r(a)).mapN((_, _) => a)
      case Or(l, r) => 
        l(a) match {
          case Validated.Valid(_) => Validated.Valid(a)
          case Validated.Invalid(e1) =>
            r(a) match {
              case Validated.Valid(_) => Validated.Valid(a)
              case Validated.Invalid(e2) => Validated.Invalid(e1 |+| e2)
            }
        }
      case True(a) => Validated.Valid(a)
      case False(e) => Validated.Invalid(e)
    }
  }

  object Predicate {
    def lift[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    final private case class True[E, A](a: A) extends Predicate[E, A]
    final private case class False[E, A](e: E) extends Predicate[E, A] 
    final private case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]
    final private case class And[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]
    final private case class Or[E, A](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]
  }

  sealed trait Check[E, A, B]{
    // import Check._
    def apply(a: A)(implicit S: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] =
      Check.Map(this, f)

    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = 
      Check.FlatMap(this, f)
  }






  object Check {
    def lift[E, A](p: Predicate[E, A]): Check[E, A, A] = Pure(p)

    private final case class Pure[E, A](p: Predicate[E, A]) extends Check[E, A, A]{
      def apply(a: A)(implicit S: Semigroup[E]): Validated[E, A] =
        p(a)
    }

    private final case class Map[E, A, B, C](
      check: Check[E, A, B],
      f: B => C
    ) extends Check[E, A, C] {
      def apply(a: A)(implicit S: Semigroup[E]): Validated[E, C] =
        check(a).map(f)
    }

    private final case class FlatMap[E, A, B, C](
      check: Check[E, A, B],
      f: B => Check[E, A, C]
    ) extends Check[E, A, C]{
      def apply(a: A)(implicit S: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => f(b)(a).toEither))
    }
  }


  //   sealed trait Check[E, A, B]{
  //   def apply(a: A)(implicit S: Semigroup[E]): Validated[E, B] = this match {
  //     case Pure(p) => p(a)
  //     case Map(check, f) => check(a).map(f)
  //   }

  //   def map[C](f: B => C): Check[E, A, C] =
  //     Check.Map(this, f)
  // }
  // object Check {

  //   private final case class Pure[E, A](p: Predicate[E, A]) extends Check[E, A, A] 

  //   private final case class Map[E, A, B, C](
  //     check: Check[E, A, B],
  //     f: B => C
  //   ) extends Check[E, A, C]
  // }

// }

















































  // trait Check[E, A, B]{
  //   def apply(value: A): Either[E, B]
  // }

  // sealed trait Predicate[E, A]{
  //   def and(x: Predicate[E, A])(y: Predicate[E, A]): Predicate[E, A]
  //   def or(x: Predicate[E,A])(y: Predicate[E, A]): Predicate[E, A]
  //

  // Inconsistent Application Between Typeclass and Datastructure
  // Either Kleisli internal or only the functions.
  // trait Check[E, A]{

  //   def run(check: Check[E, A]): Kleisli[Either[E, ?], A, A]

  //   def and(x: Check[E, A])(y: Check[E, A])(implicit S: Semigroup[E]): Check[E, A]= {
  //     val kleisli = Kleisli[Either[E, ?], A, A]{a => 
  //       (run(x)(a), run(y)(a)) match {
  //         case (Left(ex), Left(ey)) => (ex |+| ey).asLeft
  //         case (l@Left(_), Right(_)) => l
  //         case (Right(_), l@Left(_)) => l
  //         case (Right(_), Right(_)) => a.asRight
  //       }
  //     }
  //     Check.fromK(kleisli)
  //   }
  // }
  // object Check {
  //   def fromK[E, A](k: Kleisli[Either[E, ?], A, A]): Check[E, A] =
  //     new Check[E, A]{
  //       def run(check: Check[E, A]): Kleisli[Either[E, ?], A, A] = k
  //     }

  //   def fromF[E, A](f: A => Either[E, A]): Check[E, A] = {
  //     new Check[E, A]{
  //       def run(check: Check[E, A]) = Kleisli(f)
  //     }
  //   }
  // }

  // def TestChecks1: Check[List[String], Int]  = {
  //   val a = Check.fromF[List[String], Int]{int => 
  //     if (int > 2) int.asRight
  //     else List("Must be > 2").asLeft
  //   }
  //   val b = Check.fromF[List[String], Int]{int => 
  //     if (int < -2) int.asRight
  //     else List("Must be < -2").asLeft
  //   } 
  //   a.and(a)(b)
  // }



}