package io

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

object DeferredExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      d <- Deferred[IO, Int]
      f <- d.get.flatTap(i => IO.delay(println(s"completed with $i"))).start
      _ <- d.complete(42)
      i <- f.join
      _ <- IO.delay(println(i))
    } yield ExitCode.Success
}
