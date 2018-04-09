package io.chrisdavenport.scalawithcats.chapter8

import cats._
import cats.implicits._
import cats.effect._

trait UptimeClient[F[_]]{
  def getUptime(hostname: String): F[Int]
}

object UptimeClient{
  def apply[F[_]](implicit ev: UptimeClient[F]) = ev

  def getFromServer(hostname: String): IO[Int] = IO{
    println(s"$hostname")
    3
  }

  def func[F[_], A, B >: A](a: A)(F: Applicative[F]) : F[B] = Applicative[F].pure(a).widen


    // Some => Option
    def something[A](s: Some[A]): Option[A] = s

    // Option =!> Some
    def otherWay[A](o: Option[A]): Some[A] = ???

    // Functor => Covariant Functor
    
    //
    // sealed trait Animal {
    //   def moo : String
    // }
    // final case object Dog {def moo = "Bark"} extends Animal
    // final case object Cat {def moo = "Meow"} extends Animal
    // 
    //
    //
    // val l : List[Animal] = List(Dog, Cat, Dog)



  // Monad
  // F[A] => (A => F[B]) => F[B]
  // Comonad
  // F[B] => (F[B] => A) => F[A]

  // flatten: F[F[A]] => F[A] flatten 
  // trait Arrow[F[_], G[_]]{
  //   def nat[A](f: F[A]): G[A] 
  // }

  // coflatten : F[A] => F[F[A]]
  // pure: A => F[A]
  //       F[A] => A


  // def uptimeServiceId(map: Map[String, Int]): UptimeClient[Id] = new UptimeClient[Id]{
  //   def getUptime(hostname: String): Id[Int] = map.getOrElse(hostname, 0)
  // }
}

trait UptimeService[F[_]]{
  def getTotalUptime(hostnames: List[String]): F[Int]
}

object UptimeService {
  def apply[F[_]](implicit ev: UptimeService[F]) = ev
  def impl[F[_]: UptimeClient: Applicative]: UptimeService[F] = new UptimeService[F]{
    override def getTotalUptime(hostnames: List[String]): F[Int] = 
      hostnames.traverse(UptimeClient[F].getUptime).map(_.sum)
  }
}
