package io.chrisdavenport.scalawithcats.chapter8

import cats._
// import cats.implicits._

trait UptimeClient[F[_]]{
  def getUptime(hostname: String): F[Int]
}

object UptimeClient{
  def apply[F[_]](implicit ev: UptimeClient[F]) = ev

  def uptimeServiceId(map: Map[String, Int]): UptimeClient[Id] = new UptimeClient[Id]{
    def getUptime(hostname: String): Id[Int] = map.getOrElse(hostname, 0)
  }
}

trait UptimeService[F[_]]{
  def getTotalUptime(hostnames: List[String]): F[Int]
}

object UptimeService {
  def apply[F[_]](implicit ev: UptimeService[F]) = ev
}
