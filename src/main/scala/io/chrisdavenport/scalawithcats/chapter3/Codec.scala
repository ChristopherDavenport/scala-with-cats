package io.chrisdavenport.scalawithcats.chapter3

import cats.Invariant

trait Codec[A]{
  def encode(value: A): String
  def decode(value: String): A

}

object Codec {
  implicit val invariantCodec : Invariant[Codec] = new Invariant[Codec]{
    override def imap[A,B](fa: Codec[A])(f: A => B)(g: B => A): Codec[B] = new Codec[B]{
      override def encode(value: B): String = fa.encode(g(value))
      override def decode(value: String): B = f(fa.decode(value))
    }
  }

  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }
  implicit val intCodec: Codec[Int] = invariantCodec.imap(stringCodec)(_.toInt)(_.toString)
  implicit val booleanCodec: Codec[Boolean] = invariantCodec.imap(stringCodec)(_.toBoolean)(_.toString)
  case class CodecBox[A](value: A)
  implicit def codecBox[A](implicit C: Codec[A]) = invariantCodec.imap[A, CodecBox[A]](C)(CodecBox[A](_))(_.value)

}