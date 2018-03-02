package io.chrisdavenport.scalawithcats.chapter5

import cats.implicits._
import cats.data._
import cats._
import cats.effect.IO

object TransformAndRollOut {

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  // String => EitherT[IO, String, Int] = EitherT.leftT(_)
  // Int => EitherT[IO, String, Int] = EitherT.rightT(_)
  // IO[String] => EitherT[IO, String, Int] = EitherT.left(_)
  // IO[Int] => EitherT[IO, String, Int] = EitherT.right(_)
  // IO[Either[String, Int]] => EitherT[IO, String, Int] = EitherT(_)

  // String => IO[Either[String, Int]]
  // String => EitherT[IO, String, Int]


  def getPowerlevel(autobot: String): EitherT[IO, String, Int] =  {
    val autobotPowerOpt : Option[Int] = powerLevels.get(autobot)
    autobotPowerOpt
      .fold(EitherT.leftT[IO, Int](show"Invalid Autobot: Received $autobot"))(EitherT.rightT(_))
  }
    
      
  /**
  * Two autobots can perform a special move if their combined power level is greater than 15. 
  * Write a second method, canSpecialMove, that accepts the names of two allies and checks whether a special move is possible. 
  * If either ally is unavailable, fail with an appropriate error message:
  **/
  def canSpecialMove(ally1: String, ally2: String): EitherT[IO, String, Boolean] = for {
    powerLevel1 <- getPowerlevel(ally1)
    powerLevel2 <- getPowerlevel(ally2)
  } yield powerLevel1 + powerLevel2 > 15


  def tacticalReport(ally1: String, ally2: String): IO[String] = 
    canSpecialMove(ally1, ally2).map{ canSpecial => 
      if (canSpecial)
        show"$ally1 and $ally2 are ready to roll out!"
      else 
        show"$ally1 and $ally2 need a recharge."
    }.fold(identity, identity)

    def run[A](l: List[A])(implicit M: Monoid[A]): A = l.foldRight(M.empty)(M.combine)
}