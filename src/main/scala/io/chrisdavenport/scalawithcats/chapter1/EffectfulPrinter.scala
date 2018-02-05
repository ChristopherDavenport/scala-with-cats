package io.chrisdavenport.scalawithcats.chapter1

import cats.Monad
import cats.effect._

trait EffectfulPrinter[F[_]]{
  def print[A](a: A)(implicit ev: Printable[A]): F[Unit]
  def printWith[A](a: A, f: A => String): F[Unit]
}

object EffectfulPrinterSyntax {

  implicit class EffectfulSynt[F[_], A](fA: F[A]){
    def print(implicit ev: EffectfulPrinter[F], M: Monad[F], P: Printable[A]): F[Unit] = M.flatMap(fA)(a => ev.print(a))
    def printWith(f: A => String)(implicit ev: EffectfulPrinter[F], M: Monad[F]): F[Unit] = M.flatMap(fA)(a => ev.printWith(a, f)) 
  }

}

object EffectfulPrinterInstances {
  implicit def syncPrinter[F[_]: Sync] : EffectfulPrinter[F] = new EffectfulPrinter[F]{
    def print[A](a: A)(implicit ev: Printable[A]): F[Unit] = Sync[F].delay(println(ev.print(a)))
    def printWith[A](a: A, f: A => String): F[Unit] = Sync[F].delay(println(f(a)))
  }

}

