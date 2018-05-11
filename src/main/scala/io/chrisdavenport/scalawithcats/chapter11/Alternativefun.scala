package io.chrisdavenport.scalawithcats.chapter11

import cats._
import cats.kernel.CommutativeSemigroup
import cats.implicits._
import cats.data._
// import cats.effect._

object Alternativefun {

  // A , A => A
  // trait SemigroupK[F[_]]{
  //   def combineK[A](xa: F[A], ya: F[A]): F[A]
  // }

  // trait MonoidK[F[_]] extends SemigroupK[F] {
  //   def empty[A]: F[A]
  // }
  // object MonoidK {
  //   def apply[F[_]](implicit ev: MonoidK[F]) = ev
  // }

  val l = List(Either.left("Something"), Either.right(1))
  val ltup: (List[String], List[Int]) = l.separate

  

  def guard[F[_]: MonoidK: Applicative](bool: Boolean): F[Unit] = 
    if (bool) cats.Applicative[F].pure(()) else MonoidK[F].empty

  def unite[F[_]: MonoidK: Monad, G[_]: Foldable, A](fga: F[G[A]]): F[A] =
    fga.flatMap{ ga => 
      Foldable[G].foldLeft(ga, MonoidK[F].empty[A])((acc, a) => MonoidK[F].combineK(acc, Monad[F].pure(a)))
    }

  def uniteListAndVector(): Unit = {
    val l = List(NonEmptyList.of(1,2), NonEmptyList.of(4,5))
    val l2 = List(Option(1), Option(2), Option.empty[Int], Option(4))
    val output = unite(l)
    val out2 = unite(l2)
    println(output)
    println(out2)
  }


  def separate[F[_]: MonoidK, G[_, _], A, B](fgab: F[G[A, B]])(implicit FM: Monad[F], G: Bifoldable[G]): (F[A], F[B]) = {
    val as = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(Monad[F].pure, _ => MonoidK[F].empty[A])(MonoidK[F].algebra[A]))
    val bs = FM.flatMap(fgab)(gab => G.bifoldMap(gab)(_ => MonoidK[F].empty[B], Monad[F].pure)(MonoidK[F].algebra[B]))
    (as, bs)
  }

  def semigroup[A]: CommutativeSemigroup[A] = new CommutativeSemigroup[A]{
    def combine(x: A, y: A): A = {
      val _ = y
      x
    }
  }

  // IMPOSSIBLE
  // def foldableF[F[_]: Sync] : Foldable[F] = new Foldable[F]{
  // def foldLeft[A, B](fa: F[A],b: B)(f: (B, A) => B): B = fa.map(a => f(b, a))
  // def foldRight[A, B](fa: F[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???


  // }

  // def separate[F[_]: MonoidK : Monad, G[_, _] : Bifoldable, A, B](fgab: F[G[A, B]]): (F[A], F[B]) = {
  //   val as : F[A] = Monad[F].flatMap(fgab)(gab => 
  //     Bifoldable[G].bifoldMap(gab)(
  //       Monad[F].pure,
  //       _ => MonoidK[F].empty[A]
  //     )(MonoidK[F].algebra[A])
  //   )

  // }


  val optionMonoidK: MonoidK[Option] = new MonoidK[Option]{
  // Members declared in cats.MonoidK
  def empty[A]: Option[A] = Option.empty[A]
  
  // Members declared in cats.SemigroupK
  // <+>
  def combineK[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
  }
  //
  // A => F[B]
  //Kleisli[OptionT[F, ?], Request[F], Response[F]]
  // httpService1 <+> httpService2




  // List(1,2,3) ::: List(4,5,6)
  // val l : List[A] = List.empty[A]

}