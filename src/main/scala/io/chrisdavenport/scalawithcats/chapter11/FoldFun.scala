package io.chrisdavenport.scalawithcats.chapter11


import scala.annotation.tailrec

object FoldFun {
  // Option
  // List
  // Vector

  trait Foldable[F[_]]{ self => 
    import cats.Monoid
    import cats.Eval
    // Nary Values
    // 1, 2, 3, 4
    def foldLeft[A, B](fa: F[A])(b: B)(f: (B, A) => B): B

    def foldRight[A, B](fa: F[A])(b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

    def foldMonoid[A, B: Monoid](fa: F[A])(f: A => B): B =
      foldLeft[A, B](fa)(Monoid[B].empty){
        case (b, a) => Monoid[B].combine(b, f(a))
      }



  }
  object Foldable {
    def apply[F[_]](implicit ev: Foldable[F]) = ev
  }

  object ListFun {
    implicit val listFoldable: Foldable[List] = new Foldable[List] {
      import cats.Eval
      @tailrec
      def foldLeft[A, B](fa: List[A])(b: B)(f: (B, A) => B): B = fa match {
        case Nil => b
        case x :: xs => foldLeft(xs)(f(b, x))(f)
      }
      def foldRight[A, B](
        fa: List[A]
        )(b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
          case Nil => b
          case x :: xs => f(x, foldRight(xs)(b)(f))
        }
    }

    final def reverse2[A](l: List[A]): List[A] = 
      Foldable[List].foldLeft(l)(List.empty[A]){
        case (acc, a) => a :: acc
      }

    @tailrec
    final def reverse[A](l: List[A], acc: List[A] = List.empty[A]): List[A] = l match {
      case Nil => acc
      case x :: xs => reverse(xs, x :: acc)
    }

    @tailrec
    final def take[A](
      l: List[A], 
      n: Int, 
      acc: List[A] = List.empty[A]): List[A] = l match {
        case Nil => reverse(acc)
        case _ if n == 0 => reverse(acc)
        case x :: xs => take(xs, n -1, x :: acc)
      }

  }
}