package io.chrisdavenport.scalawithcats.chapter6

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

import scala.util.Try

object LoggingInPureFunctions {

  // Writer[Vector[String], ?]
  // type VectorWriter[A] = Writer[Vector[String], A]
  // List[A]
  def pureTransformation(x: Int, y: String): Writer[Vector[String], Option[Double]] = {
    for {
      _ <- Writer.tell(Vector(s"Got $y"))
    } yield Try((y.length / x).toDouble).toOption
  }

  def withPureTransformations(l: List[(Int, String)]): IO[List[Option[Double]]] = {
    val traversed: Writer[Vector[String], List[Option[Double]]] = l.traverse{case (x, y) => pureTransformation(x, y)}
    val (vec, list) : (Vector[String], List[Option[Double]]) = traversed.run
    val logging : IO[Unit] = vec.traverse_(log => IO(println(log)))
    logging *> list.pure[IO]
  }

}