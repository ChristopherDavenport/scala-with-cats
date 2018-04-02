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
