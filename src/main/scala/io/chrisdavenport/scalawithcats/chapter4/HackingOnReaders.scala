package io.chrisdavenport.scalawithcats.chapter4

import cats.implicits._
import cats.data._

object HackingOnReaders {

  case class DB(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )
  
  def findUsername(userId: Int): Reader[DB, Option[String]] = 
    Reader[DB, Option[String]](_.usernames.get(userId))
  def checkPassword(username: String, password: String): Reader[DB, Boolean] =
    Reader[DB, Boolean](_.passwords.get(username).map(_ === password).getOrElse(false))

  def checkLogin(userId: Int, password: String): Reader[DB, Boolean] = for {
    userName <- findUsername(userId)
    // If Missing False, If UserId Exists CheckPassword
    bool <- userName.fold(false.pure[Reader[DB, ?]])(checkPassword(_, password))
  } yield bool

  def checkExample(): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
    val db = DB(users, passwords)
    
    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(1, "acidburn").run(db))
    println(checkLogin(4, "davinci").run(db))


  }


}