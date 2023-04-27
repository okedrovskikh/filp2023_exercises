package crawler

import cats.effect.{ExitCode, IO, IOApp}
import crawler.HttpClient.{HttpResponse, URL}

object Run extends IOApp {
  val httpClient: HttpClient[IO] =
    url => IO.delay(HttpResponse(scala.io.Source.fromURL(url.url).mkString))

  val crawler: Crawler = new Crawler(httpClient)

  def run(args: List[String]): IO[ExitCode] =
    for {
      res <- crawler.crawl(URL("http://filp.ulanzetz.com/exercises10"))
      _   <- IO.delay(println(res))
    } yield ExitCode.Success
}
