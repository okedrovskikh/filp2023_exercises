package io

object TrampoliningExample1 {
  case class IO[A](unsafeRun: () => A) {
    self =>

    def map[B](f: A => B): IO[B] =
      IO(() => f(self.unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }

  def forever[A](action: IO[A]): IO[A] = {
    lazy val cont = forever(action)
    action.flatMap(_ => cont)
  }

  def main(args: Array[String]): Unit = {
    forever(IO(() => println("Hello world"))).unsafeRun()
  }
}

object TrampoliningExample2 {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def map[B](f: A => B): IO[B] = flatMap(a => Return(f(a)))
  }

  object IO {
    def pure[A](a: => A): IO[A] = Return(a)

    def delay[A](resume: () => A): IO[A] = Suspend(resume)
  }

  case class Return[A](a: A)                                 extends IO[A]
  case class Suspend[A](resume: () => A)                     extends IO[A]
  case class FlatMap[A, B](sub: IO[A], callback: A => IO[B]) extends IO[B]
}

object TrampoliningExample3 {
  import TrampoliningExample2._

  @scala.annotation.tailrec
  def run[A](io: IO[A]): A =
    io match {
      case Return(a) =>
        a
      case Suspend(resume) =>
        resume()

      // not tailrec implementation
      // case FlatMap(sub, callback) =>
      //   run(callback(run(sub)))

      case FlatMap(sub, callback) =>
        sub match {
          case Return(a) =>
            run(callback(a))
          case Suspend(resume) =>
            run(callback(resume()))
          case FlatMap(sub2, callback2) =>
            // (sub2.flatMap(callback2)).flatMap(callback)
            //   is equivalent
            // sub2.flatMap(x => callback2(x).flatMap(callback))
            run(sub2.flatMap(x => callback2(x).flatMap(callback)))
        }
    }
}

object TrampoliningExample4 {
  import TrampoliningExample2._
  import TrampoliningExample3._

  def forever[A](action: IO[A]): IO[A] = {
    lazy val cont = forever(action)
    action.flatMap(_ => cont)
  }

  def main(args: Array[String]): Unit = {
    run(forever(IO.delay(() => println("Hello world"))))
  }
}
