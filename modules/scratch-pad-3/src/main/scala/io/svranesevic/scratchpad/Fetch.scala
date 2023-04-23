package io.svranesevic.scratchpad

import cats.data.Ior
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import cats.effect.IOApp

final class Fetch[+T] private (private val step: Cache => IO[Result[T]]) extends AnyVal { self =>

  import Result.*

  def map[U](fn: T => U): Fetch[U] =
    Fetch { cache =>
      IO.defer {
        self.step(cache).map {
          case Done(a)           => Done(fn(a))
          case Blocked(rs, cont) => Blocked(rs, cont.map(fn))
          case f: Failed         => f
        }
      }
    }

  def flatMap[U](fn: T => Fetch[U]): Fetch[U] =
    Fetch { cache =>
      IO.defer {
        self.step(cache).flatMap {
          case Done(a)           => fn(a).step(cache)
          case Blocked(rs, cont) => Blocked(rs, cont.flatMap(fn)).pure[IO]
          case f: Failed         => f.pure[IO]
        }
      }
    }

  def zip[U](that: => Fetch[U]): Fetch[(T, U)] =
    Fetch { cache =>
      (self.step(cache) both that.step(cache)).map {
        case (Done(a), Done(b))                         => Done((a, b))
        case (Done(a), Blocked(rs, cont))               => Blocked(rs, cont.map(b => a -> b))
        case (Blocked(rs, cont), Done(b))               => Blocked(rs, cont.map(a => a -> b))
        case (Blocked(rs1, cont1), Blocked(rs2, cont2)) => Blocked(rs1 ++ rs2, cont1 zip cont2)
        case (f: Failed, _)                             => f
        case (_, f: Failed)                             => f
      }
    }

  def zipWith[U, V](that: => Fetch[U])(fn: (T, U) => V): Fetch[V] =
    self.zip(that).map(fn.tupled)

  def recover[U >: T](fn: PartialFunction[Throwable, U]): Fetch[U] =
    self.recoverWith(fn.andThen(Fetch.succeed _))

  def recoverWith[U >: T](fn: PartialFunction[Throwable, Fetch[U]]): Fetch[U] =
    Fetch { cache =>
      self.step(cache).flatMap {
        case failed: Failed    => fn.lift(failed.t).fold(failed.pure[IO])(_.step(cache))
        case Blocked(rs, cont) => Blocked[U](rs, cont.recoverWith(fn)).pure[IO]
        case done: Done[?]     => done.pure[IO]
      }
    }

  def compile(cache: Cache): IO[T] = Fetch.compile(this, cache)
}

object Fetch {

  import Result.*

  def succeed[T](t: T): Fetch[T] = Fetch(_ => Done(t).pure[IO])

  def fail[T](t: Throwable): Fetch[T] = Fetch(_ => Failed(t).pure[IO])

  def sequence[T](f: Fetch[T], fs: Fetch[T]*): Fetch[Seq[T]] =
    Fetch.sequence(f +: fs)

  def sequence[T](fs: Seq[Fetch[T]]): Fetch[Seq[T]] = {
    def cons(t: (T, Seq[T])): Seq[T] = t._1 +: t._2
    fs.toList match {
      case Nil          => Fetch.succeed(Nil)
      case head :: tail => head.zip(sequence(tail)).map(cons)
    }
  }

  def traverseU[T, U](ts: Seq[T])(fn: T => Fetch[U]): Fetch[Seq[U]] =
    Fetch.sequence(ts.map(fn))

  def traverse[T, U, F[U] <: IterableOnce[U]](ts: Seq[T])(fn: T => Fetch[F[U]]): Fetch[Seq[U]] =
    Fetch.sequence(ts.map(fn)).map(_.flatten)

  def apply[R, T](dataSource: DataSource[R, T], request: R): Fetch[T] =
    Fetch { cache =>
      def cont(status: Ref[IO, RequestStatus[T]]): IO[Result[T]] =
        status.get.map {
          case RequestStatus.Done(a)   => Result.Done(a)
          case RequestStatus.Failed(t) => Result.Failed(t)
          case RequestStatus.Pending   => throw new RuntimeException(s"impossible")
        }

      cache.get[T](request).flatMap {
        case None =>
          for {
            status <- Ref.of[IO, RequestStatus[T]](RequestStatus.Pending)
            _      <- cache.put(request, status)
            blocking = Request[R, T](request, dataSource, status)
          } yield Result.Blocked(blocking :: Nil, Fetch(_ => cont(status)))

        case Some(status) =>
          status.get.flatMap {
            case _: RequestStatus.Done[?] => cont(status)
            case _: RequestStatus.Failed  => cont(status)
            case RequestStatus.Pending    => Result.Blocked(Nil, Fetch(_ => cont(status))).pure[IO]
          }
      }
    }

  private def compile[A](f: Fetch[A], cache: Cache): IO[A] =
    f.step(cache).flatMap {
      case Result.Done(a)           => a.pure[IO]
      case Result.Failed(t)         => t.raiseError[IO, A]
      case Result.Blocked(rs, cont) => fetchBlocked(rs, cont, cache)
    }

  private def fetchBlocked[R, A](requests: Seq[Request], cont: Fetch[A], cache: Cache): IO[A] = {
    val byDataSource =
      requests
        .map(_.withRT)
        .collect { case r: Request.WithRT[R, A] => r }
        .groupBy(_.dataSource)
        .toSeq

    val fetchDataSourcesInParallel =
      byDataSource.parTraverse_ { case (dataSource, requests) =>
        val uniqueRequests = requests.groupMap(_.request)(_.status)
        fetchFromDataSource(dataSource, uniqueRequests)
      }

    fetchDataSourcesInParallel >> compile(cont, cache)
  }

  private def fetchFromDataSource[R, T](
    dataSource: DataSource[R, T],
    statusesByRequest: Map[R, Seq[Ref[IO, RequestStatus[T]]]]
  ): IO[Unit] =
    dataSource
      .fetch(statusesByRequest.keys.toSeq)
      .recoverWith { t =>
        val statuses = statusesByRequest.values.flatten.toSeq
        statuses.parTraverse_(_.set(RequestStatus.Failed(t))).as(Seq.empty)
      }
      .flatMap { results =>
        results.toSeq.parTraverse_ { case (request, result) =>
          val statuses = statusesByRequest(request)
          statuses.parTraverse_(_.set(RequestStatus.Done(result)))
        }
      }

  private def apply[T](step: Cache => IO[Result[T]]): Fetch[T] =
    new Fetch(step)
}

sealed trait Result[+T]
object Result {
  case class Blocked[T](reqs: Seq[Request], cont: Fetch[T]) extends Result[T]
  case class Done[T](a: T)                                  extends Result[T]
  case class Failed(t: Throwable)                           extends Result[Nothing]
}

sealed trait Request {
  type R
  type T

  def request: R
  def dataSource: DataSource[R, T]
  def status: Ref[IO, RequestStatus[T]]

  def withRT: Request.WithRT[R, T] = this
}

object Request {

  type WithRT[R0, T0] =
    Request {
      type R = R0
      type T = T0
    }

  def apply[R0, T0](r: R0, ds: DataSource[R0, T0], s: Ref[IO, RequestStatus[T0]]): Request.WithRT[R0, T0] =
    new Request {
      override type R = R0
      override type T = T0

      override def request: R                        = r
      override def dataSource: DataSource[R, T]      = ds
      override def status: Ref[IO, RequestStatus[T]] = s
    }
}

sealed trait RequestStatus[+A]
object RequestStatus {
  case object Pending             extends RequestStatus[Nothing]
  case class Done[A](a: A)        extends RequestStatus[A]
  case class Failed(t: Throwable) extends RequestStatus[Nothing]
}

trait DataSource[R, T] {
  def fetch(reqs: Seq[R]): IO[Map[R, T]]
}

trait Cache {
  def get[T](key: Any): IO[Option[Ref[IO, RequestStatus[T]]]]
  def put[T](key: Any, value: Ref[IO, RequestStatus[T]]): IO[Unit]
}
object Cache {

  def default: Cache =
    new Cache {
      private val map: scala.collection.concurrent.Map[Any, Ref[IO, RequestStatus[?]]] =
        scala.collection.concurrent.TrieMap.empty[Any, Ref[IO, RequestStatus[?]]]

      override def get[T](key: Any): IO[Option[Ref[IO, RequestStatus[T]]]] =
        IO(map.get(key).map(_.asInstanceOf[Ref[IO, RequestStatus[T]]]))

      override def put[T](key: Any, value: Ref[IO, RequestStatus[T]]): IO[Unit] =
        IO(map.putIfAbsent(key, value.asInstanceOf[Ref[IO, RequestStatus[?]]]))
    }
}

object FetchMain extends IOApp.Simple {

  final case class Tweet(id: Int, text: String)

  sealed trait TweetRequest
  object TweetRequest {
    case class ById(id: Int) extends TweetRequest
  }

  case object TweetsRepository extends DataSource[TweetRequest, Option[Tweet]] {
    override def fetch(reqs: Seq[TweetRequest]): IO[Map[TweetRequest, Option[Tweet]]] = {
      val ids   = reqs.map { case TweetRequest.ById(id) => id }
      val query = s"SELECT * FROM tweets WHERE id IN (${ids.mkString(", ")})"

      val response = ids.map(id => id -> Tweet(id, s"tweet $id").some).toMap
      val result   = reqs.map { case r @ TweetRequest.ById(id) => r -> response.getOrElse(id, none) }

      IO
        .println(s" => Fetching tweets: $query")
        .as(result.toMap.widen)
    }
  }

  val getTweetIds: Fetch[Seq[TweetRequest.ById]] =
    Fetch.succeed(Seq(1, 2, 3, 4, 5).map(TweetRequest.ById.apply))

  def getTweetById(id: Int): Fetch[Option[Tweet]] =
    getTweet(TweetRequest.ById(id))

  def getTweet(req: TweetRequest): Fetch[Option[Tweet]] =
    Fetch(TweetsRepository, req)

  val followingTweets: Fetch[Seq[Tweet]] =
    for {
      ids    <- getTweetIds
      tweets <- Fetch.traverse(ids)(getTweet)
    } yield tweets

  val trendingTweets: Fetch[Seq[Tweet]] =
    for {
      ids    <- Fetch.succeed(Seq(2, 6, 7, 8, 2))
      tweets <- Fetch.traverse(ids)(getTweetById)
    } yield tweets

  val tweets: Fetch[Seq[Tweet]] = followingTweets.zipWith(trendingTweets)(_ ++ _)

  val fetchTweets = tweets.compile(Cache.default) >>= IO.println

  val stackSafeFlatMap: IO[Unit] =
    (0 to 100_000)
      .map(Fetch.succeed(_))
      .foldLeft(Fetch.succeed(0L)) { case (query1, query2) =>
        for {
          acc <- query1
          i   <- query2
        } yield acc + i
      }
      .compile(Cache.default)
      .void

  val stackSafeSequence: IO[Unit] =
    Fetch
      .sequence(
        List.fill(100_000)(Fetch.succeed(0L))
      )
      .compile(Cache.default)
      .void

  override def run: IO[Unit] = stackSafeFlatMap *> stackSafeSequence *> fetchTweets
}
