package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = this match {
      case Right(_) => this
      case Left(_) =>
        other match {
          case Right(_) => other
          case _        => this
        }
    }

    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = this match {
      case Left(x) => Left(x)
      case Right(x) =>
        other match {
          case Left(y)  => Left(y)
          case Right(y) => Right(f(x, y))
        }
    }

    def map[C](f: B => C): Either[A, C] = this match {
      case Left(x)  => Left(x)
      case Right(x) => Right(f(x))
    }

    def flatMap[X >: A, C](f: B => Either[X, C]): Either[X, C] = this match {
      case Left(x)  => Left(x)
      case Right(x) => f(x)
    }

    def toOption: Option[B] =
      this match {
        case Left(_)  => None
        case Right(x) => Option(Right(x).get)
      }
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] =
      option match {
        case None    => Left(a)
        case Some(x) => Right(x)
      }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      list.foldLeft[Either[E, List[B]]](Right(List()))((acc, e) =>
        acc match {
          case Right(x) =>
            f(e) match {
              case Right(y) => Right(x :+ y)
              case Left(y)  => Left(y)
            }
          case Left(x) => Left(x)
        }
      )

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = traverse(list)(identity)
  }

}
