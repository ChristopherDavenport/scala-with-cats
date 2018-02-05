package io.chrisdavenport.scalawithcats

object Main {

  // import io.chrisdavenport.scalawithcats.chapter2._
  // import SemigroupInstances._

  import cats.effect._  

  def javaWrapper[F[_]: Sync]: F[Unit] = {
    Sync[F].delay(println("Blah!"))
  }

  def main(args: Array[String]): Unit = {
    // val result = Semigroup[Int => Int].combine({x: Int =>  x + 1}, {x: Int => x * 10}).apply(6)
    // println(result)

   def  eff = for {
      _ <- javaWrapper[IO]
      res <- javaWrapper[IO]
    } yield res

    eff[IO].unsafeRunSync()
    
  }

}