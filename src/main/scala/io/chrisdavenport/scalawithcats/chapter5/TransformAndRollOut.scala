package io.chrisdavenport.scalawithcats.chapter5

import cats.implicits._
import cats.data._
import cats.effect.IO

object TransformAndRollOut {

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerlevel(autobot: String): EitherT[IO, String, Int] = 
    powerLevels.get(autobot).fold(EitherT.leftT[IO, Int](show"Invalid Autobot: Received $autobot"))(EitherT.rightT(_))

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
}