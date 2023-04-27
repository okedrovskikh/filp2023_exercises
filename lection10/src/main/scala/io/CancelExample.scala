package io

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object CancelExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      fiber <- (IO.sleep(10.seconds) *> IO.cancelBoundary *> IO.delay(println("hello"))).start
      _     <- fiber.cancel
    } yield ExitCode.Success
}

object CancelExample1 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- (IO.race(IO.sleep(10.seconds).flatMap(_ => IO.delay(println("hello"))),
        IO.sleep(5.seconds).flatMap(_ => IO.delay(println("bye")))))
    } yield ExitCode.Success
}
