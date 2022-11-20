package io.svranesevic.scratchpad.wip

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all._
import cats.effect.IOApp

final case class Fetch[+T](private val step: Cache => IO[Result[T]]) extends AnyVal {

  import Result._

  def map[U](fn: T => U): Fetch[U] = Fetch.map(this, fn)

  def flatMap[U](fn: T => Fetch[U]): Fetch[U] = Fetch.flatMap(this, fn)

  def zip[U](that: Fetch[U]): Fetch[(T, U)] = Fetch.zip(this, that)

  def zipWith[U, V](that: Fetch[U])(fn: (T, U) => V): Fetch[V] = Fetch.map2(this, that)(fn)

  def recover[U >: T](fn: PartialFunction[Throwable, U]): Fetch[U] =
    recoverWith(fn.andThen(Fetch.succeed _))

  def recoverWith[U >: T](fn: PartialFunction[Throwable, Fetch[U]]): Fetch[U] =
    Fetch.recover(this, fn)

  def compile(cache: Cache): IO[T] = Fetch.compile(this, cache)
}

object Fetch {

  import Result._

  private def map[T, U](f: Fetch[T], fn: T => U): Fetch[U] = Fetch[U] { cache =>
    f.step(cache).map {
      case Done(a)           => Done(fn(a))
      case Blocked(rs, cont) => Blocked(rs, cont.map(fn))
      case f: Failed         => f
    }
  }

  private def flatMap[T, U](f: Fetch[T], fn: T => Fetch[U]): Fetch[U] = Fetch { cache =>
    IO.defer {
      f.step(cache).flatMap {
        case Done(a)           => fn(a).step(cache)
        case Blocked(rs, cont) => Blocked(rs, cont.flatMap(fn)).pure[IO]
        case f: Failed         => f.pure[IO]
      }
    }
  }

  private def map2[T, U, V](a: Fetch[T], b: Fetch[U])(fn: (T, U) => V): Fetch[V] =
    Fetch.zip(a, b).map(fn.tupled)

  private def recover[T, U >: T](f: Fetch[T], fn: PartialFunction[Throwable, Fetch[U]]): Fetch[U] = Fetch { cache =>
    f.step(cache).flatMap {
      case failed: Failed =>
        fn.lift(failed.t) match {
          case Some(recovered) => recovered.step(cache)
          case None            => failed.pure[IO]
        }
      case Blocked(rs, cont) => Blocked[U](rs, cont.recoverWith(fn)).pure[IO]
      case done: Done[T]     => done.pure[IO]
    }
  }

  def succeed[T](t: T): Fetch[T] = Fetch(_ => Done(t).pure[IO])

  def fail[T](t: Throwable): Fetch[T] = Fetch(_ => Failed(t).pure[IO])

  def zip[A, B](a: Fetch[A], b: Fetch[B]): Fetch[(A, B)] = Fetch { cache =>
    (a.step(cache) both b.step(cache)).map {
      case (Done(a), Done(b))                         => Done((a, b))
      case (Done(a), Blocked(rs, cont))               => Blocked(rs, cont.map(b => a -> b))
      case (Blocked(rs, cont), Done(b))               => Blocked(rs, cont.map(a => a -> b))
      case (Blocked(rs1, cont1), Blocked(rs2, cont2)) => Blocked(rs1 ++ rs2, cont1 zip cont2)
      case (f: Failed, _)                             => f
      case (_, f: Failed)                             => f
    }
  }

  def sequence[T](f: Fetch[T], fs: Fetch[T]*): Fetch[Seq[T]] =
    Fetch.sequence(f +: fs)

  def sequence[T](fs: Seq[Fetch[T]]): Fetch[Seq[T]] = {
    // Pretty, but stack unsafe
    // def cons[T](t: (T, Seq[T])): Seq[T] = t._1 +: t._2
    // fs.toList match {
    //   case Nil          => Fetch.succeed(Nil)
    //   case head :: tail => head.zip(sequence(tail)).map(cons)
    // }
    val builder = Seq.newBuilder[T]
    builder.sizeHint(fs.size)

    fs
      .foldLeft(Fetch.succeed(builder)) { case (fs, f) =>
        (fs zip f).flatMap { case (ts, t) => Fetch.succeed(ts.addOne(t)) }
      }
      .map(_.result())
  }

  def traverseU[T, U](ts: Seq[T])(fn: T => Fetch[U]): Fetch[Seq[U]] =
    Fetch.sequence(ts.map(fn))

  def traverse[T, U, F[U] <: IterableOnce[U]](ts: Seq[T])(fn: T => Fetch[F[U]]): Fetch[Seq[U]] =
    Fetch.sequence(ts.map(fn)).map(_.flatten)

  def apply[R, T](dataSource: DataSource[R, T], request: R): Fetch[T] =
    Fetch { cache =>
      def cont[T](status: Ref[IO, RequestStatus[T]]): IO[Result[T]] =
        status.get.map {
          case RequestStatus.Done(a)   => Result.Done(a)
          case RequestStatus.Failed(t) => Result.Failed(t)
          case RequestStatus.Pending   => throw new RuntimeException(s"impossible")
        }

      cache.get(request).flatMap { ref: Option[Ref[IO, RequestStatus[T]]] =>
        ref match {
          case None =>
            for {
              status <- Ref.of[IO, RequestStatus[T]](RequestStatus.Pending)
              _ <- cache.put(request, status)
              blocking = Request.Blocking[R, T](request, dataSource, status)
            } yield Result.Blocked(blocking :: Nil, Fetch(_ => cont(status)))

          case Some(status) =>
            status.get.flatMap {
              case _: RequestStatus.Done[T] => cont(status)
              case _: RequestStatus.Failed  => cont(status)
              case RequestStatus.Pending    => Result.Blocked(Nil, Fetch(_ => cont(status))).pure[IO]
            }
        }
      }
    }

  def compile[A](f: Fetch[A], cache: Cache): IO[A] =
    f.step(cache).flatMap {
      case Result.Done(a)           => a.pure[IO]
      case Result.Failed(t)         => t.raiseError[IO, A]
      case Result.Blocked(rs, cont) => fetchBlocked(rs, cont, cache)
    }

  private def fetchBlocked[A](reqs: Seq[Request], cont: Fetch[A], cache: Cache): IO[A] = {
    val byDataSource = reqs.toList.groupBy { case Request.Blocking(_, dataSource, _) => dataSource }.toList
    val fetchDataSourcesInParallel =
      byDataSource
        .parTraverse_ { case (dataSource, reqs) =>
          val (requests, statuses) =
            reqs.unzip { case Request.Blocking(request, _, status) => (request, status) }

          fetchFromDataSource(dataSource, requests, statuses)
        }

    fetchDataSourcesInParallel >> compile(cont, cache)
  }

  private def fetchFromDataSource[R, T](
      dataSource: DataSource[R, T],
      requests: Seq[R],
      statuses: Seq[Ref[IO, RequestStatus[T]]]
  ): IO[Unit] =
    dataSource
      .fetch(requests)
      .recoverWith { t =>
        statuses.parTraverse_(_.set(RequestStatus.Failed(t))).as(Seq.empty)
      }
      .flatMap { results =>
        (statuses zip results).parTraverse_ { case (status, result) =>
          status.set(RequestStatus.Done(result))
        }
      }
}

sealed trait Result[+T]
object Result {
  case class Blocked[T](reqs: Seq[Request], cont: Fetch[T]) extends Result[T]
  case class Done[T](a: T) extends Result[T]
  case class Failed(t: Throwable) extends Result[Nothing]
}

sealed trait Request
object Request {
  case class Blocking[R, T](request: R, dataSource: DataSource[R, T], status: Ref[IO, RequestStatus[T]]) extends Request
}

sealed trait RequestStatus[+A]
object RequestStatus {
  case object Pending extends RequestStatus[Nothing]
  case class Done[A](a: A) extends RequestStatus[A]
  case class Failed(t: Throwable) extends RequestStatus[Nothing]
}

trait DataSource[R, A] {
  def fetch(reqs: Seq[R]): IO[Seq[A]]
}

trait Cache {
  def get[T](key: Any): IO[Option[Ref[IO, RequestStatus[T]]]]
  def put[T](key: Any, value: Ref[IO, RequestStatus[T]]): IO[Unit]
}
object Cache {

  def default: Cache =
    new Cache {
      private val map: scala.collection.concurrent.Map[Any, Ref[IO, RequestStatus[_]]] =
        scala.collection.concurrent.TrieMap.empty[Any, Ref[IO, RequestStatus[_]]]

      override def get[T](key: Any): IO[Option[Ref[IO, RequestStatus[T]]]] =
        IO(map.get(key).map(_.asInstanceOf[Ref[IO, RequestStatus[T]]]))

      override def put[T](key: Any, value: Ref[IO, RequestStatus[T]]): IO[Unit] =
        IO(map.putIfAbsent(key, value.asInstanceOf[Ref[IO, RequestStatus[_]]]))
    }
}

object FetchMain extends IOApp.Simple {

  final case class Tweet(id: Int, text: String)

  sealed trait TweetRequest
  object TweetRequest {
    case class ById(id: Int) extends TweetRequest
  }

  case object TweetsRepository extends DataSource[TweetRequest, Option[Tweet]] {
    override def fetch(reqs: Seq[TweetRequest]): IO[Seq[Option[Tweet]]] = {
      val ids = reqs.collect { case TweetRequest.ById(id) => id }
      val query = s"SELECT * FROM tweets WHERE id IN (${ids.mkString(", ")})"

      IO
        .println(s" => Fetching tweets: $query")
        .as(ids.map(id => Tweet(id, s"tweet $id").some))
    }
  }

  val getTweetIds: Fetch[Seq[TweetRequest.ById]] =
    Fetch.succeed(Seq(1, 2, 3, 4, 5).map(TweetRequest.ById))

  def getTweetById(id: Int): Fetch[Option[Tweet]] =
    getTweet(TweetRequest.ById(id))

  def getTweet(req: TweetRequest): Fetch[Option[Tweet]] =
    Fetch(TweetsRepository, req)

  sealed trait ProfileRequest
  object ProfileRequest {
    case class ById(id: Int) extends ProfileRequest
  }

  val followingTweets: Fetch[Seq[Tweet]] =
    for {
      ids <- getTweetIds
      tweets <- Fetch.traverse(ids)(getTweet)
    } yield tweets

  val trendingTweets: Fetch[Seq[Tweet]] =
    for {
      ids <- Fetch.succeed(Seq(2, 6, 7, 8))
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
          i <- query2
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
