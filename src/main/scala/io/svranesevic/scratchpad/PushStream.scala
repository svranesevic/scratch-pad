package io.svranesevic.scratchpad

import java.util.concurrent.Semaphore
import scala.collection.mutable

object PushStream {

  trait Subscriber[E] {

    def handle(e: Option[E]): Unit = e match {
      case Some(e) => onEvent(e)
      case None    => onComplete()
    }

    def onEvent(e: E): Unit = ()

    def onComplete(): Unit = ()
  }

  abstract class Subscribable[E](
      private val subscribers: mutable.Set[Subscriber[E]] = mutable.Set.empty[Subscriber[E]]
  ) {

    def subscribe(s: Subscriber[E]): Unit = subscribers.synchronized {
      subscribers += s
      onSubscription()
    }

    def subscribe(f: E => Unit): Unit =
      subscribe(new Subscriber[E] {
        override def onEvent(e: E): Unit = f(e)
      })

    def subscribe(f: E => Unit, g: () => Unit): Unit =
      subscribe(new Subscriber[E] {
        override def onEvent(e: E): Unit = f(e)
        override def onComplete(): Unit = g()
      })

    def forEachSubscriber(f: Subscriber[E] => Unit): Unit = subscribers.synchronized {
      subscribers.foreach(f)
    }

    protected def onSubscription(): Unit = ()
  }

  sealed case class Stream[E]() extends Subscribable[E] {

    def publish(e: E): Unit = dispatch(Some(e))
    def complete(): Unit = dispatch(None)

    protected def dispatch(e: Option[E]): Unit = forEachSubscriber(_.handle(e))

    def drain(): Unit = foreach(_ => ())
    def drainToSeq(): Seq[E] = foldLeft(Seq.empty[E], (acc: Seq[E], e: E) => acc :+ e)

    def foldLeft[O](initial: O, onEvent: (O, E) => O): O = {
      var out = initial
      val obj = new Semaphore(0)

      subscribe(e => out = onEvent(out, e), () => obj.release())

      obj.acquire()
      out
    }

    def foreach(f: E => Unit): Unit = subscribe(f)

    def map[V](f: E => V): Stream[V] = new Stream.Map[E, V](this, f)
    def flatMap[V](f: E => Stream[V]): Stream[V] = new Stream.FlatMap(this, f)
    def filter(f: E => Boolean): Stream[E] = new Stream.Filter(this, f)
    def collect[V](pf: PartialFunction[E, V]): Stream[V] = new Stream.Collect(this, pf.lift)
    def tap(f: E => Unit): Stream[E] = new Stream.Tap(this, f)

    def ++(that: Stream[E]): Stream[E] = zip(that)
    def zip(that: Stream[E]): Stream[E] = new Stream.Zip(this, that)
  }

  object Stream {

    def from[E](es: E*): Stream[E] = new Stream[E] {
      override protected def onSubscription(): Unit =
        (es.map(Some(_)) :+ None).foreach(dispatch)
    }

    private abstract class Transformer[I, O](source: Stream[I]) extends Stream[O] with Subscriber[I] {
      override protected def onSubscription(): Unit = source.subscribe(this)
      override def onComplete(): Unit = dispatch(None)
    }

    private final class Map[E, V](source: Stream[E], f: E => V) extends Transformer[E, V](source) {
      override def onEvent(e: E): Unit = publish(f(e))
    }

    private final class FlatMap[E, V](source: Stream[E], f: E => Stream[V]) extends Transformer[E, V](source) {
      override def onEvent(e: E): Unit =
        f(e).subscribe(publish(_))
    }

    private final class Filter[E](source: Stream[E], f: E => Boolean) extends Transformer[E, E](source) {
      override def onEvent(e: E): Unit = if (f(e)) publish(e)
    }

    private final class Collect[E, V](source: Stream[E], pf: E => Option[V]) extends Transformer[E, V](source) {
      override def onEvent(e: E): Unit = pf.apply(e).foreach(publish)
    }

    private final class Tap[E](source: Stream[E], f: E => Unit) extends Transformer[E, E](source) {
      override def onEvent(e: E): Unit = {
        f(e)
        publish(e)
      }
    }

    private final class Zip[E](left: Stream[E], right: Stream[E]) extends Stream[E] with Subscriber[E] {
      override protected def onSubscription(): Unit = {
        left.subscribe(this)
        right.subscribe(this)
      }
      override def onComplete(): Unit = dispatch(None)
      override def onEvent(e: E): Unit = publish(e)
    }
  }
}
