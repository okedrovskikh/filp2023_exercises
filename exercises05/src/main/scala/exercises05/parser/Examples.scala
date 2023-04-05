package exercises05.parser

import exercises05.either.EitherCombinators._
import exercises05.parser.Error.{Banned, InvalidId, InvalidName, InvalidPassport}

import scala.util.matching.Regex

object Examples {
  private val passportRegex = new Regex("(\\d{4}) (\\d{6})")

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
    */
  def transformToOption(rawUser: RawUser): Option[User] =
    transformToEither(rawUser).toOption

  private def transformToOptionPassport(passport: Option[String]): Option[Option[Passport]] =
    passport match {
      case None => Some(None)
      case Some(x) =>
        passportRegex.findFirstMatchIn(x) match {
          case None => None
          case Some(y) =>
            (y.group(1).toLongOption, y.group(2).toLongOption) match {
              case (_, None) | (None, _)        => None
              case (Some(series), Some(number)) => Some(Some(Passport(series, number)))
            }
        }
    }

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for {
      _          <- if (rawUser.banned) Left(Banned) else Right(true)
      id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
      passport   <- Either.fromOption(transformToOptionPassport(rawUser.passport))(InvalidPassport)
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport)
}
