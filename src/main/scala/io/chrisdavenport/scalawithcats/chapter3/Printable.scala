package io.chrisdavenport.scalawithcats.chapter3

import cats._

trait Printable[A] {
  def format(value: A): String
}

object Printable {

  implicit val contraPrinter: Contravariant[Printable] = new Contravariant[Printable]{
    override def contramap[A, B](fa: Printable[A])(f: B => A): Printable[B] = new Printable[B]{
      override def format(value: B): String = fa.format(f(value))
    }
  }
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String ="\"" + value + "\""
  }
  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }
  final case class Box[A](value: A)
  implicit def boxPrintable[A](implicit P: Printable[A]) = contraPrinter.contramap[A, Box[A]](P)(_.value)

  implicit class PrintableOps[A](a: A)(implicit P: Printable[A]){
    def format: String = P.format(a)
  }

}