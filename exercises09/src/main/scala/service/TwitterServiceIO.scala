package service

import cats.effect.IO
import cats.syntax.all._
import service.domain.GetTweetResponse.{Found, NotFound}
import service.domain._

import twitter.domain.TwitterError._
import twitter.TwitterApi
import twitter.domain._

import scala.util.{Failure, Success}

// Воспользуйтесь синтаксисом map, recover, traverse из cats.syntax.all_
class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {
  def tweet(user: User, text: String): IO[TweetId] = {

    IO.async_(cb => api.tweet(user, text)(x => cb(x.toEither)))
  }

  def like(user: User, tweetId: TweetId): IO[Unit] = {
    IO.async_((cb: Either[Throwable, Unit] => Unit) => api.like(user, tweetId)(x => cb(x.toEither))).recover {
      case LikeAlreadyExistError => Right()
    }
  }

  def unlike(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_((cb: Either[Throwable, Unit] => Unit) => api.unlike(user, tweetId)(x => cb(x.toEither))).recover {
      case LikeNotExistError => Right()
    }

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] =
    IO.async_(cb =>
      api.get(tweetId)(x =>
        cb(x match {
          case Success(value) => Right(Found(value))
          case Failure(_)     => Right(NotFound(tweetId))
        })
      )
    )

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] =
    for {
      tweets <- ids.traverse(getTweet)
      res = tweets.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo]))((acc, elem) =>
        elem match {
          case Found(info)  => acc.copy(found = acc.found + info)
          case NotFound(id) => acc.copy(notFound = acc.notFound + id)
        }
      )
    } yield res

}
