package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    override def toOption(a: RawUser): Option[User] =
      for {
        firstName  <- a.firstName
        secondName <- a.secondName
        id         <- a.id.toLongOption
      } yield User(id, UserName(firstName, secondName, a.thirdName))

    override def toEither(a: RawUser): Either[Error, User] =
      for {
        firstName  <- a.firstName.toRight(InvalidName)
        secondName <- a.secondName.toRight(InvalidName)
        id         <- a.id.toLongOption.toRight(InvalidId)
      } yield User(id, UserName(firstName, secondName, a.thirdName))
  }
}

object TransformerSyntax {

  implicit class TransformerOps[A](private val a: A) extends AnyVal {
    def transformToOption[B](implicit ev: Transformer[A, B]): Option[B]        = ev.toOption(a)
    def transformToEither[B](implicit ev: Transformer[A, B]): Either[Error, B] = ev.toEither(a)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
