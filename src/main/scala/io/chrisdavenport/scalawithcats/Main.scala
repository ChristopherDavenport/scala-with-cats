package io.chrisdavenport.scalawithcats

// import chapter3._
// import chapter9._
import cats.implicits._
// import cats._
import cats.effect._
import fs2._
import scala.concurrent.ExecutionContext.Implicits.global


object Main {

  def main(args: Array[String]): Unit = {
    // def initStream: Stream[IO, Int] = Stream.unfoldEval(0){i => 
    // //   if (i >= 5){
    // //     Sync[IO].raiseError[Option[(Int, Int)]](new Throwable("Boo!"))
    // //   } else {
    // //     Sync[IO].delay((i, i + 1).some)
    // //   }
    // // }.handleErrorWith(_ => initStream)
    
    Stream.range(0, 100)
    .covary[IO]
    .map(i => Stream(i).covary[IO])
    .joinUnbounded

    // .evalMap(i => IO(println(i)))
      .compile
      .foldMonoid
      .flatMap(i => IO(println(i)))
      .unsafeRunSync

    // val p = FoldMap2.parFoldMap[Vector, IO, Int, String](Vector(1,2,3,4,5), 3)(_ => Thread.currentThread().getName + "! ")

    // p.attempt.flatMap(s => IO(println(s))).unsafeRunSync
    // println(p)

    // val traversed = TraverseDiscovery.myTraverse[IO, Int, Int](List(1,2,3)){ int => 
    //   if (int % 2 == 0) IO.raiseError[Int](new Throwable("Boom!")) else IO(println(int)).as(int)
    // }
    // traversed.void.unsafeRunSync
  }

}