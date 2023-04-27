package io

import cats.effect.{ExitCode, IO, IOApp}

object App3 {

  trait Console[F[_]] {
    def getStringLine: F[String]
    def putStringLine(line: String): F[Unit]
  }

  object Console {
    def apply[F[_]: Console]: Console[F] = implicitly[Console[F]]

    def putStringLine[F[_]: Console](line: String): F[Unit] =
      Console[F].putStringLine(line)

    def getStringLine[F[_]: Console]: F[String] =
      Console[F].getStringLine
  }

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  object Random {
    def apply[F[_]: Random]: Random[F] = implicitly[Random[F]]

    def nextInt[F[_]: Random](upper: Int): F[Int] =
      Random[F].nextInt(upper)
  }

  import Console._
  import Random._
  import cats.Monad
  import cats.syntax.all._ // импортировать синтаксис всех cats-тайпклассов
  // import cats.instances.option._ // импортировать инстансы cats-тайпклассов для Option

  def gameLoop[F[_]: Monad: Random: Console](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStringLine(s"Dear $name, please guess a number from 1 to 5:")
      input <- getStringLine
      _ <- input.toIntOption.fold(
        putStringLine("You did not enter a number")
      ) { guess =>
        if (guess == num) {
          putStringLine(s"You guessed right, $name!")
        } else {
          putStringLine(s"You guessed wrong, $name! The number was: $num")
        }
      }
      cont <- checkContinue(name)
      _    <- if (cont) gameLoop(name) else ().pure
    } yield ()

  def checkContinue[F[_]: Monad: Console](name: String): F[Boolean] =
    for {
      _     <- putStringLine(s"Do you want to continue, $name?")
      input <- getStringLine
      result <- input match {
        case "y" => true.pure
        case "n" => false.pure
        case _   => checkContinue(name)
      }
    } yield result

  def mainApp[F[_]: Monad: Random: Console]: F[Unit] =
    for {
      _    <- putStringLine("What is your name?")
      name <- getStringLine
      _    <- putStringLine(s"Hello, $name, welcome to the game!")
      _    <- gameLoop(name)
    } yield ()
}

object MainApp3 extends IOApp {
  import io.App3.{Console, Random, mainApp}
  import cats.syntax.all._

  implicit val consoleIO: Console[IO] = new Console[IO] {
    override def getStringLine: IO[String] =
      IO(scala.Console.in.readLine())

    override def putStringLine(line: String): IO[Unit] =
      IO(scala.Console.out.println(line))
  }

  implicit val randomIO: Random[IO] = new Random[IO] {
    override def nextInt(upper: Int): IO[Int] =
      IO(scala.util.Random.nextInt(upper))
  }

  override def run(args: List[String]): IO[ExitCode] =
    mainApp[IO].as(ExitCode.Success)
}
