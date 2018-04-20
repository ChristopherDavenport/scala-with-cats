package io.chrisdavenport.scalawithcats.chapter10



object AccState {

  import cats.Eval
  sealed trait Acc[S, A]{
    def apply(s: S): Eval[(S, A)] = this match {
      case Acc.Pure(a) => Eval.later((s, a))
      case Acc.Map(acc, f) => acc(s).map{ case (s, a) => (s, f(a))}
      
      case Acc.FlatMap(acc, f) => acc(s).flatMap{ case (s, a) => f(a)(s) }
      
      
      case Acc.Modify(acc, f) => acc(s).map{ case (s, a) => (f(s), a)}
      case Acc.Inspect(acc, f) => acc(s).map{ case (s, _) => (s, f(s))}
    }

    def pure(a: A): Acc[S, A] = Acc.Pure(a)

    def map[B](f: A => B): Acc[S, B] = Acc.Map(this, f)

    def flatMap[B](f: A => Acc[S, B]): Acc[S, B] = Acc.FlatMap(this, f)

    def modify(f: S => S): Acc[S, A] = Acc.Modify[S, A](this, f)
    def inspect[B](f: S => B): Acc[S, B] = Acc.Inspect(this, f)

  }
  object Acc {

    val f = for {
      _ <- modify[Int](_ + 1)
      value <- pure(3)
      _ <- modify[Int](_ + value)
      result <- get
    } yield result

    val coolAcc : Acc[Int, Int] = FlatMap[Int, Unit, Int](
      Modify[Int, Unit](Pure[Int, Unit](()), _ + 1),
      {_: Unit => 
        FlatMap[Int, Int, Int](
          Pure(3),
          {value =>
            FlatMap(
              Modify[Int, Unit](Pure[Int, Unit](()), _ + value),
              {_: Unit  => 
                Inspect[Int, Unit, Int](Pure(()), identity)
              }
            )
            
          }
        )
      }
    )

    // (8, 3)
    val v : (Int, Int) = coolAcc(7).value
    
    trait Weirdness[A]
    final case object Some extends Weirdness[Int]
    final case class MyInt(int: Int) extends Weirdness[Int]




    final case class Pure[S, A](a: A) extends Acc[S, A]
    final case class Map[S, A, B](acc: Acc[S, A], f: A => B) extends Acc[S, B]
    final case class FlatMap[S, A, B](acc: Acc[S, A], f: A => Acc[S, B]) extends Acc[S, B]
    final case class Modify[S, A](acc: Acc[S, A], f: S => S) extends Acc[S, A]
    final case class Inspect[S, A, B](acc: Acc[S, A], f: S => B) extends Acc[S, B]

        
    // def simple[S, A](f: S => (S, A)): Acc[S, A] = 
    def pure[S, A](a: A): Acc[S, A] = Pure(a)
    
    // Combinators
    def modify[S](f: S => S): Acc[S, Unit] = Modify[S, Unit](Pure(()), f)

    def inspect[S, A, T](f: S => T): Acc[S, T] = Inspect[S, Unit, T](Pure(()), f)
    def get[S, A]: Acc[S, S] = inspect(identity)

    def set[S](s: S): Acc[S, Unit] = modify[S](_ => s)
  }

}