package io.svranesevic.scratchpad.wip

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.syntax.all._
import cats.syntax.all._
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.util.Success
import scala.util.Failure

final case class Fetch[+T](result: Cache => Result[T]) {

  import Result._

  def map[U](fn: T => U): Fetch[U] =
    Fetch[U] { cache =>
      result(cache) match {
        case Done(a)             => Done(fn(a))
        case Blocked(reqs, cont) => Blocked(reqs, cont.map(fn))
        case t: Throw            => t
      }
    }

  def flatMap[U](fn: T => Fetch[U]): Fetch[U] =
    Fetch { cache =>
      result(cache) match {
        case Done(a)             => fn(a).result(cache)
        case Blocked(reqs, cont) => Blocked(reqs, cont.flatMap(fn))
        case t: Throw            => t
      }
    }

  def zip[U](that: Fetch[U]): Fetch[(T, U)] =
    Fetch { cache =>
      (this.result(cache), that.result(cache)) match {
        case (Done(a), Done(b))                         => Done((a, b))
        case (Done(a), Blocked(br, cont))               => Blocked(br, cont.map(b => a -> b))
        case (Blocked(br, cont), Done(b))               => Blocked(br, cont.map(a => a -> b))
        case (Blocked(br1, cont1), Blocked(br2, cont2)) => Blocked(br1 ++ br2, cont1 zip cont2)
        case (t: Throw, _)                              => t
        case (_, t: Throw)                              => t
      }
    }

  def combine[U, V](that: Fetch[U])(fn: (T, U) => V): Fetch[V] =
    this.zip(that).map(fn.tupled)
}

object Fetch {

  def lift[T](t: T): Fetch[T] = Fetch(_ => Result.Done(t))

  def sequence[T](fs: Seq[Fetch[T]]): Fetch[Seq[T]] =
    fs
      .foldLeft(Fetch.lift(Seq.newBuilder[T])) { case (fs, f) =>
        (fs zip f).flatMap { case (s, a) => Fetch.lift(s.addOne(a)) }
      }
      .map(_.result())

  def traverse[T, U](ts: Seq[T])(fn: T => Fetch[U]): Fetch[Seq[U]] =
    Fetch.sequence(ts.map(fn))

  def fromDataSource[R, T](request: R, dataSource: DataSource[R, T]): Fetch[T] =
    Fetch { cache =>
      cache.get(request) match {
        case None =>
          val promise = Promise[T]()
          cache.put(request, promise)
          val blocked = Request.Blocked[R, T](request, dataSource, promise)
          Result.Blocked(blocked :: Nil, Fetch(_ => cont(promise)))

        case Some(promise) =>
          if (promise.isCompleted) cont(promise)
          else Result.Blocked(Nil, Fetch(_ => cont(promise)))
      }
    }

  private def cont[T](p: Promise[T]): Result[T] =
    p.future.value match {
      case Some(scala.util.Success(value)) => Result.Done(value)
      case Some(scala.util.Failure(t))     => Result.Throw(t)
      case None                            => throw new RuntimeException("impossible")
    }

  def run[A](f: Fetch[A], cache: Cache)(implicit ec: ExecutionContext): Future[A] = {
    f.result(cache) match {
      case Result.Done(a)           => Future.successful(a)
      case Result.Throw(a)          => Future.failed(a)
      case Result.Blocked(br, cont) => fetch(br, cont, cache)
    }
  }

  private def fetch[A](brs: Seq[Request], cont: Fetch[A], cache: Cache)(implicit ec: ExecutionContext): Future[A] = {
    val groups = brs.toList.groupBy { case Request.Blocked(_, ds, _) => ds }.toList

    Future
      .traverse(groups) { case (dataSource, group) =>
        val (requests, promises) = group.unzip { case Request.Blocked(request, _, promise) => (request, promise) }

        dataSource.fetch(requests).onComplete {
          case Success(results) =>
            (promises zip results).map { case (promise, result) =>
              promise.complete(Success(result))
            }

          case Failure(t) => promises.map(_.complete(Failure(t)))
        }

        Future.sequence(promises.map(_.future))
      }
      .flatMap(_ => run(cont, cache))
  }
}

sealed trait Result[+T]
object Result {
  case class Done[T](a: T) extends Result[T]
  case class Blocked[T](reqs: Seq[Request], cont: Fetch[T]) extends Result[T]
  case class Throw(t: Throwable) extends Result[Nothing]
}

trait Request
object Request {
  case class Blocked[R, A](request: R, dataSource: DataSource[R, A], promise: Promise[A]) extends Request
}

trait DataSource[R, A] {
  def fetch(reqs: Seq[R]): Future[Seq[A]]
}

trait Cache {
  def get[A](key: Any): Option[Promise[A]]
  def put[A](key: Any, value: Promise[A]): Unit
}
object Cache {

  def inMemory: Cache =
    new Cache {
      private val map: scala.collection.concurrent.Map[Any, Promise[_]] =
        scala.collection.concurrent.TrieMap.empty[Any, Promise[_]]

      override def get[A](key: Any): Option[Promise[A]] =
        map.get(key).map(_.asInstanceOf[Promise[A]])

      override def put[A](key: Any, value: Promise[A]): Unit =
        map.putIfAbsent(key, value)
    }
}

object FetchMain extends App {

  sealed trait TweetRequest
  object TweetRequest {
    case class ById(id: Int) extends TweetRequest
    case class ByKey(key: String) extends TweetRequest
  }

  case object TweetsRepository extends DataSource[TweetRequest, String] {
    override def fetch(reqs: Seq[TweetRequest]): Future[Seq[String]] = {
      println(s"Fetching tweet(s): ${reqs.mkString("[", ", ", "]")}")
      Future.successful {
        reqs.map {
          case (TweetRequest.ById(id))   => s"tweet id=$id"
          case (TweetRequest.ByKey(key)) => s"tweet key=$key"
        }
      }
    }
  }

  val getTweetIds: Fetch[Seq[TweetRequest.ById]] =
    Fetch.lift(Seq(1, 2, 3, 4, 5).map(TweetRequest.ById(_)))

  val getTweetKeys: Fetch[Seq[TweetRequest.ByKey]] =
    Fetch.lift(Seq("quadra", "dadra", "due").map(TweetRequest.ByKey(_)))

  def getTweet(req: TweetRequest): Fetch[String] =
    Fetch.fromDataSource(req, TweetsRepository)

  def getTweetById(id: Int): Fetch[String] =
    getTweet(TweetRequest.ById(id))

  val fetch =
    for {
      ids <- getTweetIds
      keys <- getTweetKeys
      tweets <- Fetch.traverse(ids ++ keys)(getTweet)
    } yield tweets

  val fetch2 =
    for {
      ids <- Fetch.lift(Seq(2, 6, 7, 8))
      tweets <- Fetch.traverse(ids)(getTweetById)
    } yield tweets

  val both = fetch.combine(fetch2)(_ ++ _)

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.concurrent.Await

  val result = Await.result(Fetch.run(both, Cache.inMemory), 1.minute)
  println(result)
}
