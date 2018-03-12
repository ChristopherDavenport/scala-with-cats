package io.chrisdavenport.scalawithcats.chapter6

import cats._
import cats.data._
import cats.implicits._

object ValidateMyExistence {
  // Say you are writing a password change application.
  // You are given a username, an old password, and a new password
  case class PasswordChangeRequest(username: String, oldPassword: String, newPassword: String)
  // We have a set of rules we would like to enforce on this data,
  // and would like to ensure that all of them are correct,
  // or we will return a list of all of the failures
  // Rules:
  // 1. Username MUST be all lowercase
  // 2. Username MUST NOT be empty
  // 3. Passwords MUST be atleast 8 characters long
  // 4. Passwords MUST contain a Number in the first 8 characters
  // 5. Passwords MUST contain a Letter in the first 8 characters
  // 6. Passwords MUST NOT contain characters ' \ or :

  class Username private[ValidateMyExistence] (v: String){
    val value: String = v
  }

  private def validateUsername(username: String): ValidatedNel[String, Username] =
    (usernameLowercase(username), usernameNotEmpty(username))
      .mapN((_, _) => new Username(username))

  private def usernameLowercase(username: String): ValidatedNel[String, Unit] = 
    Validated.condNel[String, Unit](
      username.matches(".*[a-z]+.*"), 
      (), 
      "Username Not All Lowercase"
    )
  private def usernameNotEmpty(username: String): ValidatedNel[String, Unit] = 
    Validated.condNel[String, Unit](
      !username.isEmpty,
      (),
      "Username is Empty"
    )

  class Password private[ValidateMyExistence] (v: String){
    val value: String = v
  }

  private sealed trait PasswordType
  private object PasswordType {
    implicit val showPassword: Show[PasswordType] = Show.show[PasswordType]{
      case OldPassword => "Old Password"
      case NewPassword => "New Password"
    }
    case object OldPassword extends PasswordType
    case object NewPassword extends PasswordType
  }
  
  private def validatePassword(password: String, passwordType: PasswordType): ValidatedNel[String, Password] = 
    (
      passwordLongerThanEight(password, passwordType),
      numberInFirstEightChars(password, passwordType),
      letterInFirstEightChars(password, passwordType),
      doesNotContainInvalidCharacters(password, passwordType)
    ).mapN((_, _, _, _) => new Password(password))

  private def passwordLongerThanEight(password: String, passwordType: PasswordType): ValidatedNel[String, Unit] = 
    Validated.condNel[String, Unit](
      password.length >= 8,
      (),
      show"PasswordType: $passwordType - Password Not Longer Than 8 Characters"
    )

  private def numberInFirstEightChars(password: String, passwordType: PasswordType): ValidatedNel[String, Unit] =
    Validated.condNel[String, Unit](
      password.take(8).matches(".*\\d+.*"), 
      (), 
      show"PasswordType: $passwordType - No Number in First 8 Characters"
    )
  
  private def letterInFirstEightChars(password: String, passwordType: PasswordType): ValidatedNel[String, Unit] = 
    Validated.condNel(
      password.take(8).matches(".*[a-zA-Z]+.*"), 
      (), 
      show"PasswordType: $passwordType - No Alpha Character in First 8 Characters"
    )

  private def doesNotContainInvalidCharacters(s: String, passwordType: PasswordType): ValidatedNel[String, Unit] = (
    Validated.condNel[String, Unit](!s.contains("\""), (), show"""PasswordType: $passwordType - Contains Invalid Character: '"'"""),
    Validated.condNel[String, Unit](!s.contains("\'"), (), show"""PasswordType: $passwordType - Contains Invalid Character: "'" """),
    Validated.condNel[String, Unit](!s.contains(":"), (), show"""PasswordType: $passwordType - Contains Invalid Character: ':'""")
    ).mapN((_, _, _) => ())

  case class ValidatedPasswordChangeRequest(username: Username, oldPassword: Password, newPassword: Password)

  def validatePasswordChangeRequest(p: PasswordChangeRequest): ValidatedNel[String, ValidatedPasswordChangeRequest] = 
    (
      validateUsername(p.username),
      validatePassword(p.oldPassword, PasswordType.OldPassword),
      validatePassword(p.newPassword, PasswordType.NewPassword)
    ).mapN(ValidatedPasswordChangeRequest)

}