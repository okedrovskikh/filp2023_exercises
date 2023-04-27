package io

import cats.Monad
import cats.data.State
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import io.App4.{Console, Game, Random}

object App4 {
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

  class Game[F[_]: Monad: Random: Console] {
    import Console._
    import Random._

    def gameLoop(name: String): F[Unit] =
      for {
        num   <- nextInt[F](5).map(_ + 1)
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

    def checkContinue(name: String): F[Boolean] =
      for {
        _     <- putStringLine(s"Do you want to continue, $name?")
        input <- getStringLine
        result <- input match {
          case "y" => true.pure
          case "n" => false.pure
          case _   => checkContinue(name)
        }
      } yield result

    def main: F[Unit] =
      for {
        _    <- putStringLine[F]("What is your name?")
        name <- getStringLine[F]
        _    <- putStringLine[F](s"Hello, $name, welcome to the game!")
        _    <- gameLoop(name)
      } yield ()
  }
}

object MainApp4 extends IOApp {
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
    new Game[IO].main.as(ExitCode.Success)
}

object App4Test {
  import App4._

  case class TestData(input: List[String], output: List[String], nums: List[Int]) {
    def nextInt: (TestData, Int) =
      (this.copy(nums = nums.tail), nums.head)

    def putStringLine(line: String): (TestData, Unit) =
      (this.copy(output = line :: output), ())

    def getStringLine: (TestData, String) =
      (this.copy(input = input.tail), input.head)
  }

  type Test[A] = State[TestData, A]

  def mainTest: Test[Unit] = {
    implicit val randomTestIO: Random[Test] = new Random[Test] {
      override def nextInt(upper: Int): Test[Int] =
        State(_.nextInt)
    }

    implicit val consoleTestIO: Console[Test] = new Console[Test] {
      override def getStringLine: Test[String] =
        State(_.getStringLine)

      override def putStringLine(line: String): Test[Unit] =
        State(_.putStringLine(line))
    }

    new Game[Test].main
  }

  def runTest(data: TestData): List[String] =
    mainTest.run(data).value._1.output.reverse

  val testData: TestData =
    TestData(
      input = "John A De Goes" :: "1" :: "n" :: Nil,
      output = Nil,
      nums = 0 :: Nil
    )

  def main(args: Array[String]): Unit =
    println(runTest(testData).mkString("\n"))
}
